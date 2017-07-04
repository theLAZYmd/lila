package lila.round

import scala.concurrent.duration._
import chess.{ Color, Status }
import lila.game.actorApi.{ AbortedBy, FinishGame }
import lila.game.{ Game, GameRepo, Pov }
import lila.hub.actorApi.map.Tell
import lila.i18n.I18nKey.{ Select => SelectI18nKey }
import lila.playban.PlaybanApi
import lila.round.actorApi.round.BugFinish
import lila.user.{ User, UserRepo }

private[round] final class Finisher(
    messenger: Messenger,
    perfsUpdater: PerfsUpdater,
    playban: PlaybanApi,
    notifier: RoundNotifier,
    crosstableApi: lila.game.CrosstableApi,
    bus: lila.common.Bus,
    casualOnly: Boolean,
    getSocketStatus: Game.ID => Fu[actorApi.SocketStatus],
    val roundMap: akka.actor.ActorSelection
) {

  def abort(pov: Pov)(implicit proxy: GameProxy): Fu[Events] = {
    pov.game.bugGameId.foreach(bgid => roundMap ! Tell(bgid, BugFinish(None, _.Aborted)))
    apply(pov.game, _.Aborted) >>- {
      getSocketStatus(pov.gameId) foreach { ss =>
        playban.abort(pov, ss.colorsOnGame)
      }
      bus.publish(AbortedBy(pov), 'abortGame)
    }
  }

  def rageQuit(game: Game, winner: Option[Color])(implicit proxy: GameProxy): Fu[Events] = {
    game.bugGameId.foreach(roundMap ! Tell(_, BugFinish(winner.map(!_), _.Timeout)))
    apply(game, _.Timeout, winner) >>-
      winner.?? { color => playban.rageQuit(game, !color) }
  }

  def outOfTime(game: Game)(implicit proxy: GameProxy): Fu[Events] = {
    import lila.common.PlayApp
    if (!PlayApp.startedSinceSeconds(60) && (game.movedAt isBefore PlayApp.startedAt)) {
      logger.info(s"Aborting game last played before JVM boot: ${game.id}")
      game.bugGameId.foreach(roundMap ! Tell(_, BugFinish(None, _.Aborted)))
      other(game, _.Aborted, none)
    }
    else {
      val winner = Some(!game.player.color) filterNot { color =>
        game.toChess.board.variant.insufficientWinningMaterial(game.toChess.situation.board, color)
      }
      game.bugGameId.foreach(roundMap ! Tell(_, BugFinish(winner, _.Outoftime)))
      apply(game, _.Outoftime, winner) >>-
        winner.?? { color => playban.sittingOrGood(game, !color) }
    }
  }

  def bug(
    game: Game,
    status: Status.type => Status,
    winner: Option[Color] = None
  )(implicit proxy: GameProxy): Fu[Events] = {
    apply(game, status, winner) >>- playban.other(game, status, winner)
  }

  def other(
    game: Game,
    status: Status.type => Status,
    winner: Option[Color] = None,
    message: Option[SelectI18nKey] = None
  )(implicit proxy: GameProxy): Fu[Events] = {
    game.bugGameId.foreach(bgId => if (status(Status) != Status.NoStart) roundMap ! Tell(bgId, BugFinish(winner, status)))
    apply(game, status, winner, message) >>- playban.other(game, status, winner)
  }

  private def apply(
    game: Game,
    makeStatus: Status.type => Status,
    winner: Option[Color] = None,
    message: Option[SelectI18nKey] = None
  )(implicit proxy: GameProxy): Fu[Events] = {
    val status = makeStatus(Status)
    val prog = game.finish(status, winner)
    if (game.nonAi && game.isCorrespondence) Color.all foreach notifier.gameEnd(prog.game)
    lila.mon.game.finish(status.name)()
    casualOnly.fold(
      GameRepo unrate prog.game.id inject prog.game.copy(mode = chess.Mode.Casual),
      fuccess(prog.game)
    ) flatMap { g =>
        proxy.save(prog) >>
          GameRepo.finish(
            id = g.id,
            winnerColor = winner,
            winnerId = winner flatMap (g.player(_).userId),
            status = prog.game.status
          ) >>
          UserRepo.pair(
            g.whitePlayer.userId,
            g.blackPlayer.userId
          ).zip {
            // because the game comes from the round GameProxy,
            // it doesn't have the tvAt field set
            // so we fetch it from the DB
            GameRepo hydrateTvAt g
          } flatMap {
            case ((whiteO, blackO), g) => {
              val finish = FinishGame(g, whiteO, blackO)
              updateCountAndPerfs(finish) inject {
                message foreach { messenger.system(g, _) }
                GameRepo game g.id foreach { newGame =>
                  bus.publish(finish.copy(game = newGame | g), 'finishGame)
                }
                prog.events
              }
            }
          }
      }
  } >>- proxy.invalidate

  private def updateCountAndPerfs(finish: FinishGame): Funit =
    (!finish.isVsSelf && !finish.game.aborted) ?? {
      (finish.white |@| finish.black).tupled ?? {
        case (white, black) =>
          crosstableApi add finish.game zip perfsUpdater.save(finish.game, white, black)
      } zip
        (finish.white ?? incNbGames(finish.game)) zip
        (finish.black ?? incNbGames(finish.game)) void
    }

  private def incNbGames(game: Game)(user: User): Funit = game.finished ?? {
    val totalTime = (game.hasClock && user.playTime.isDefined) ?? game.durationSeconds
    val tvTime = totalTime ifTrue game.metadata.tvAt.isDefined
    val result =
      if (game.winnerUserId has user.id) 1
      else if (game.loserUserId has user.id) -1
      else 0
    UserRepo.incNbGames(user.id, game.rated, game.hasAi,
      result = result,
      totalTime = totalTime,
      tvTime = tvTime).void
  }
}
