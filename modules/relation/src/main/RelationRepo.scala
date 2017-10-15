package lila.relation

import reactivemongo.api.ReadPreference
import reactivemongo.bson._

import lila.db.dsl._

private[relation] object RelationRepo {

  // dirty
  private val coll = Env.current.coll

  def following(userId: ID) = relating(userId, Follow)

  def partnering(userId: ID) = relating(userId, Follow, true)

  def blockers(userId: ID) = relaters(userId, Block)

  def blocking(userId: ID) = relating(userId, Block)

  def followersFromSecondary(userId: ID) = relaters(userId, Follow, ReadPreference.secondaryPreferred)

  def followingLike(userId: ID, term: String): Fu[List[ID]] = {
    val id = term.toLowerCase
    if (id.isEmpty) fuccess(Nil)
    else coll.distinctWithReadPreference[String, List]("u2", $doc(
      "u1" -> userId,
      "u2".$regex("^" + id + ".*$", ""),
      "r" -> Follow
    ).some,
      ReadPreference.secondaryPreferred)
  }

  private def relaters(userId: ID, relation: Relation, rp: ReadPreference = ReadPreference.primary): Fu[Set[ID]] =
    coll.distinctWithReadPreference[String, Set]("u1", $doc(
      "u2" -> userId,
      "r" -> relation
    ).some, rp)

  private def relating(userId: ID, relation: Relation, partnering: Boolean = false): Fu[Set[ID]] = {
    val keyChar = partnering.fold("p", "u")
    coll.distinct[String, Set](keyChar + "2", $doc(
      keyChar + "1" -> userId,
      "r" -> relation
    ).some)
  }

  def partner(u1: ID, u2: ID): Funit = {
    unpartnerAll(u1) //might allow multi-partnering in the future that would allow for a partnering whitelist
    save(u1, u2, Follow, true)
  }

  def unpartner(u1: ID, u2: ID): Funit = remove(u1, u2, true)

  def follow(u1: ID, u2: ID): Funit = save(u1, u2, Follow)

  def unfollow(u1: ID, u2: ID): Funit = remove(u1, u2)

  def block(u1: ID, u2: ID): Funit = save(u1, u2, Block)

  def unblock(u1: ID, u2: ID): Funit = remove(u1, u2)

  def unfollowAll(u1: ID): Funit = coll.remove($doc("u1" -> u1)).void

  def unpartnerAll(u1: ID): Funit = coll.remove($doc("p1" -> u1)).void

  private def save(u1: ID, u2: ID, relation: Relation, partnering: Boolean = false): Funit = {
    val keyChar = partnering.fold("p", "u")
    coll.update(
      $id(makeId(u1, u2, partnering)),
      $doc(keyChar + "1" -> u1, keyChar + "2" -> u2, "r" -> relation),
      upsert = true
    ).void
  }

  def remove(u1: ID, u2: ID, partnering: Boolean = false): Funit = coll.remove($id(makeId(u1, u2, partnering))).void

  def drop(userId: ID, relation: Relation, nb: Int) =
    coll.find(
      $doc("u1" -> userId, "r" -> relation),
      $doc("_id" -> true)
    )
      .list[Bdoc](nb).map {
        _.flatMap { _.getAs[String]("_id") }
      } flatMap { ids =>
        coll.remove($inIds(ids)).void
      }

  def makeId(u1: String, u2: String, partnering: Boolean = false) = partnering.fold(s"$u1>$u2", s"$u1/$u2")
}
