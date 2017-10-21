package lila.tournament
package arena

import chess.variant.Bughouse
import lila.tournament.{ PairingSystem => AbstractPairingSystem }
import lila.user.UserRepo

import scala.util.Random

private[tournament] object PairingSystem extends AbstractPairingSystem {
  type P = (String, String)

  case class Data(
      tour: Tournament,
      lastOpponents: Pairing.LastOpponents,
      ranking: Map[String, Int],
      onlyTwoActivePlayers: Boolean,
      partnersOp: Option[Map[String, String]]
  ) {

    val isFirstRound = lastOpponents.hash.isEmpty && tour.isRecentlyStarted
  }

  // if waiting users can make pairings
  // then pair all users
  def createPairings(tour: Tournament, usersVal: WaitingUsers, ranking: Ranking, relationApi: lila.relation.RelationApi, activeUserIds: Set[String]): Fu[Pairings] = {
    for {
      lastOpponents <- PairingRepo.lastOpponents(tour.id, usersVal.all, Math.min(120, usersVal.size * 4))
      onlyTwoActivePlayers <- (tour.nbPlayers > 20).fold(
        fuccess(false),
        PlayerRepo.countActive(tour.id).map(2==)
      )
      partnersOp <- {
        if (tour.variant == Bughouse) {
          usersVal.all.iterator.map(user =>
            relationApi.fetchPartner(user).map(_.map(user -> _))).sequenceFu.map(_.flatten.toMap).map(mapping =>
            Some(closeMap(mapping, activeUserIds)))
        }
        else fuccess(None)
      }
      users = partnersOp match {
        case Some(partners) => usersVal.filter(partners.get(_).forall(usersVal.all.contains(_)))
        case None => usersVal
      }
      data = Data(tour, lastOpponents, ranking, onlyTwoActivePlayers, partnersOp)
      preps <- if (tour.variant == Bughouse) bugOnlyEven(data, users)
      else if (data.isFirstRound) evenOrAll(data, users)
      else makePreps(data, users.waiting) flatMap {
        case Nil => fuccess(Nil)
        case _ => makePreps(data, users.all)
      }
      pairings <- prepsToPairings(preps, partnersOp)
    } yield pairings
  }.chronometer.logIfSlow(500, pairingLogger) { pairings =>
    s"createPairings ${url(tour.id)} ${pairings} pairings"
  }.result

  private def evenOrAll(data: Data, users: WaitingUsers) =
    makePreps(data, users.evenNumber) flatMap {
      case Nil if users.isOdd => makePreps(data, users.all)
      case x => fuccess(x)
    }

  private def bugOnlyEven(data: Data, users: WaitingUsers) =
    makePreps(data, users.bugEvenNumber(data.partnersOp))

  private val maxGroupSize = 44

  private def makePreps(data: Data, users: List[String]): Fu[List[Pairing.Prep]] = {
    import data._
    if (users.size < 2) fuccess(Nil)
    else PlayerRepo.rankedByTourAndUserIds(tour.id, users, ranking) map { idles =>
      if (data.tour.isRecentlyStarted) tour.variant match {
        case Bughouse => bugPairings(tour, idles, data, (t, i, _) => naivePairings(t, i))
        case _ => naivePairings(tour, idles)
      }
      else if (tour.variant != Bughouse && idles.size > maxGroupSize) { //Change this if bughouse ever becomes popular
        // make sure groupSize is even with / 4 * 2
        val groupSize = (idles.size / 4 * 2) atMost maxGroupSize
        smartPairings(data, idles take groupSize) :::
          smartPairings(data, idles drop groupSize take groupSize)
      }
      else if (idles.size > 1) tour.variant match {
        case Bughouse => bugPairings(tour, idles, data, (_, i, d) => smartPairings(d, i))
        case _ => smartPairings(data, idles)
      }
      else Nil
    }
  }.chronometer.mon(_.tournament.pairing.prepTime).logIfSlow(200, pairingLogger) { preps =>
    s"makePreps ${url(data.tour.id)} ${users.size} users, ${preps.size} preps"
  }.result

  private def prepsToPairings(preps: List[Pairing.Prep], partnersOp: Option[Map[String, String]] = None): Fu[List[Pairing]] =
    if (preps.size < 50) preps.map { prep =>
      UserRepo.firstGetsWhite(prep.user1.some, prep.user2.some) map prep.toPairing
    }.sequenceFu.map { pairings =>
      partnersOp match {
        case None => pairings
        case Some(partners) =>
          var pairingsVar = pairings
          var bugOrderedPairings: Pairings = Nil

          bugColorFlipParity.fold(partners.toList.reverse, partners.toList).foreach {
            case (partner1, partner2) => {
              popFirstIdMatch(pairingsVar, partner1) match {
                case (tempPairings, Some(matchedPairing1)) => {
                  popFirstIdMatch(tempPairings, partner2) match {
                    case (newPairings, Some(matchedPairing2)) => {
                      var mp1 = matchedPairing1
                      if (matchedPairing1.users.indexOf(partner1) ==
                        matchedPairing2.users.indexOf(partner2)) mp1 = mp1.reverseColors
                      pairingsVar = newPairings
                      bugOrderedPairings = mp1 :: matchedPairing2 :: bugOrderedPairings
                    }
                    case _ =>
                      pairingLogger.warn(s"Only one partner has a pairing")
                  }
                }
                case _ => Unit
              }
            }
          }

          if (pairings.nonEmpty) bugColorFlipParity = !bugColorFlipParity
          bugOrderedPairings ::: pairingsVar
      }
    }
    else fuccess {
      preps.map(_ toPairing Random.nextBoolean)
    }

  private def naivePairings(tour: Tournament, players: RankedPlayers): List[Pairing.Prep] =
    players grouped 2 collect {
      case List(p1, p2) => Pairing.prep(tour, p1.player, p2.player)
    } toList

  private def smartPairings(data: Data, players: RankedPlayers): List[Pairing.Prep] = players.size match {
    case x if x < 2 => Nil
    case x if x <= 10 => OrnicarPairing(data, players)
    case _ => AntmaPairing(data, players)
  }

  private def bugPairings(
    tour: Tournament,
    players: RankedPlayers,
    data: Data,
    pairingOp: (Tournament, RankedPlayers, Data) => List[Pairing.Prep]
  ): List[Pairing.Prep] = {
    data.partnersOp match {
      case Some(partners) => {
        val partClosed = closeMap(partners, players.map(_.player.userId).toSet)
        val uniqueParts = (partClosed.map(rel => Set(rel._1, rel._2)).toList.toSet.toList: List[Set[String]]).map(set => {
          val it = Random.shuffle(set.toList).iterator
          it.next -> it.next
        }).toMap
        if (players.length % 4 != 0) pairingLogger.warn(s"Bughouse: Number of players mod 4 is non-zero!")
        players.partition(rp => partClosed.contains(rp.player.userId)) match {
          case (parts, singles) => {
            if (parts.isEmpty) pairingOp(tour, singles, data)
            else parts.partition(rp => uniqueParts.contains(rp.player.userId)) match {
              case (leftPartners, rightPartners) => {
                singles.splitAt(singles.length / 2) match {
                  case (leftSingles, rightSingles) => {
                    pairingOp(tour, (rightPartners ::: rightSingles).sortBy(_.rank), data) match {
                      case Nil => {
                        pairingLogger.warn(s"Not able to pair Bughouse 'right' side!")
                        Nil
                      }
                      case rightPairings => {
                        var leftPartnersVar = leftPartners
                        var leftPairings: List[Pairing.Prep] = Nil
                        rightPairings.foreach { pairing =>
                          for {
                            u1part <- partClosed.get(pairing.user1)
                            u2part <- partClosed.get(pairing.user2)
                          } yield {
                            leftPairings = Pairing.Prep(tour.id, u1part, u2part) :: leftPairings
                            leftPartnersVar =
                              leftPartnersVar.filterNot(rp => rp.player.userId == u1part || rp.player.userId == u2part)
                          }
                        }
                        val leftTotal = leftPartnersVar ::: leftSingles
                        if (!leftTotal.isEmpty) pairingOp(tour, (leftTotal).sortBy(_.rank), data) match {
                          case Nil => {
                            pairingLogger.warn(s"Not able to pair Bughouse 'left' side!")
                            Nil
                          }
                          case lp => lp ::: leftPairings ::: rightPairings
                        }
                        else leftPairings ::: rightPairings
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      case None => {
        pairingLogger.warn(s"No partner map for bugPairings method!")
        Nil
      }
    }
  }

  private def closeMap(mapping: Map[String, String], set: Set[String]): Map[String, String] = //closes map wrt a set. e.g. removes entries of the mapping that take it outside the given set
    mapping.filter(entry => set.contains(entry._2))

  private var bugColorFlipParity = true

  private def popFirstIdMatch(list: List[Pairing], id: String): (List[Pairing], Option[Pairing]) = list match {
    case Nil => (Nil, None)
    case head :: tail => {
      if (head.user1 == id || head.user2 == id) (tail, Some(head))
      else {
        val res = popFirstIdMatch(tail, id)
        (head :: res._1, res._2)
      }
    }
  }

  private[arena] def url(tourId: String) = s"https://lichess.org/tournament/$tourId"

  /* Was previously static 1000.
     * By increasing the factor for high ranked players,
     * we increase pairing quality for them.
     * The higher ranked, and the more ranking is relevant.
     * For instance rank 1 vs rank 5
     * is better thank 300 vs rank 310
     * This should increase leader vs leader pairing chances
     *
     * top rank factor = 2000
     * bottom rank factor = 300
     */
  private[arena] def rankFactorFor(players: RankedPlayers): (RankedPlayer, RankedPlayer) => Int = {
    val maxRank = players.map(_.rank).max
    (a, b) => {
      val rank = Math.min(a.rank, b.rank)
      300 + 1700 * (maxRank - rank) / maxRank
    }
  }
}
