package controllers

import play.api.libs.json.Json
import lila.api.Context
import lila.app._
import lila.common.paginator.{ AdapterLike, Paginator, PaginatorJson }
import lila.relation.Related
import lila.user.{ UserRepo, User => UserModel }
import views._

object Relation extends LilaController {

  private def env = Env.relation
  private def notifyApi = lila.notify.Env.current.api

  private def renderActions(userId: String, mini: Boolean)(implicit ctx: Context) =
    (ctx.userId ?? { env.api.fetchRelation(_, userId) }) zip
      (ctx.userId ?? { env.api.fetchPartnerRelation(_, userId) }) zip
      (ctx.userId ?? { env.api.fetchPartnerRelation(userId, _) }) zip
      (ctx.isAuth ?? { Env.pref.api followable userId }) zip
      (ctx.userId ?? { env.api.fetchBlocks(userId, _) }) flatMap {
        case relation ~ partnerRel ~ invPartnerRel ~ followable ~ blocked => {
          negotiate(
            html = fuccess(Ok(mini.fold(
              html.relation.mini(userId, blocked = blocked, followable = followable, relation = relation, partnerRelation = partnerRel, invPartnerRelation = invPartnerRel),
              html.relation.actions(userId, relation = relation, blocked = blocked, followable = followable, partnerRelation = partnerRel, invPartnerRelation = invPartnerRel)
            ))),
            api = _ => fuccess(Ok(Json.obj(
              "followable" -> followable,
              "following" -> relation.contains(true),
              "blocking" -> relation.contains(false)
            )))
          )
        }
      }

  def follow(userId: String) = Auth { implicit ctx => me =>
    env.api.follow(me.id, UserModel normalize userId).nevermind >> renderActions(userId, getBool("mini"))
  }

  def unfollow(userId: String) = Auth { implicit ctx => me =>
    env.api.unfollow(me.id, UserModel normalize userId).nevermind >> renderActions(userId, getBool("mini"))
  }

  def partner(userId: String) = Auth { implicit ctx => me =>
    for {
      invPartRel <- ctx.userId ?? { env.api.fetchPartnerRelation(userId, _) }
      toCancel <- ctx.userId ?? { env.api.fetchPartner(_) }
      invCancelRel <- (toCancel zip ctx.userId).headOption ?? { p => env.api.fetchPartnerRelation(p._1, p._2) }
      _ <- env.api.partner(me.id, UserModel normalize userId).nevermind
      render <- renderActions(userId, getBool("mini"))
    } yield {
      invPartRel match {
        case None => ctx.userId.foreach(notify(_, userId, false))
        case Some(_) => ctx.userId.foreach(notify(_, userId, true))
      }
      invCancelRel match {
        case None => Unit
        case Some(_) => (toCancel zip ctx.userId).headOption.foreach(p =>
          notify(p._2, p._1, false, true))
      }
      render
    }
  }

  def unpartner(userId: String) = Auth { implicit ctx => me =>
    for {
      invPartRel <- ctx.userId ?? { env.api.fetchPartnerRelation(userId, _) }
      _ <- env.api.unpartner(me.id, UserModel normalize userId).nevermind
      render <- renderActions(userId, getBool("mini"))
    } yield {
      invPartRel match {
        case None => Unit
        case Some(_) => ctx.userId.foreach(notify(_, userId, false, true))
      }
      render
    }
  }

  def decPartner(userId: String) = Auth { implicit ctx => me =>
    removeNotification(userId, me.id)
  }

  def block(userId: String) = Auth { implicit ctx => me =>
    env.api.block(me.id, UserModel normalize userId).nevermind >> renderActions(userId, getBool("mini"))
  }

  def unblock(userId: String) = Auth { implicit ctx => me =>
    env.api.unblock(me.id, UserModel normalize userId).nevermind >> renderActions(userId, getBool("mini"))
  }

  def following(username: String, page: Int) = Open { implicit ctx =>
    Reasonable(page, 20) {
      OptionFuResult(UserRepo named username) { user =>
        RelatedPager(env.api.followingPaginatorAdapter(user.id), page) flatMap { pag =>
          negotiate(
            html = env.api countFollowers user.id map { nbFollowers =>
            Ok(html.relation.following(user, pag, nbFollowers))
          },
            api = _ => Ok(jsonRelatedPaginator(pag)).fuccess
          )
        }
      }
    }
  }

  def followers(username: String, page: Int) = Open { implicit ctx =>
    Reasonable(page, 20) {
      OptionFuResult(UserRepo named username) { user =>
        RelatedPager(env.api.followersPaginatorAdapter(user.id), page) flatMap { pag =>
          negotiate(
            html = env.api countFollowing user.id map { nbFollowing =>
            Ok(html.relation.followers(user, pag, nbFollowing))
          },
            api = _ => Ok(jsonRelatedPaginator(pag)).fuccess
          )
        }
      }
    }
  }

  private def jsonRelatedPaginator(pag: Paginator[Related]) = {
    import lila.user.JsonView.nameWrites
    import lila.relation.JsonView.relatedWrites
    import lila.common.PimpedJson._
    Json.obj("paginator" -> PaginatorJson(pag.mapResults { r =>
      relatedWrites.writes(r) ++ Json.obj(
        "online" -> Env.user.isOnline(r.user.id).option(true),
        "perfs" -> r.user.perfs.bestPerfType.map { best =>
          lila.user.JsonView.perfs(r.user, best.some)
        }
      ).noNull
    }))
  }

  def blocks(page: Int) = Auth { implicit ctx => me =>
    Reasonable(page, 20) {
      RelatedPager(env.api.blockingPaginatorAdapter(me.id), page) map { pag =>
        html.relation.blocks(me, pag)
      }
    }
  }

  private def RelatedPager(adapter: AdapterLike[String], page: Int)(implicit ctx: Context) = Paginator(
    adapter = adapter mapFutureList followship,
    currentPage = page,
    maxPerPage = 30
  )

  private def followship(userIds: Seq[String])(implicit ctx: Context): Fu[List[Related]] =
    UserRepo usersFromSecondary userIds.map(UserModel.normalize) flatMap { users =>
      (ctx.isAuth ?? { Env.pref.api.followableIds(users map (_.id)) }) flatMap { followables =>
        users.map { u =>
          ctx.userId ?? { env.api.fetchRelation(_, u.id) } map { rel =>
            lila.relation.Related(u, none, followables(u.id), rel)
          }
        }.sequenceFu
      }
    }

  private def notify(invitedBy: String, invitee: String, requested: Boolean, canceled: Boolean = false): Funit = {
    import lila.notify.{ Notification, InvitedToPartner }
    notifyApi addNotification Notification.make(
      Notification.Notifies(invitee),
      InvitedToPartner(invitedBy, requested, canceled)
    )
  }

  private def removeNotification(invitedBy: String, invitee: String): Funit = {
    import lila.notify.{ Notification, InvitedToPartner }
    import lila.db.dsl.$doc
    notifyApi.remove(Notification.Notifies(invitee), $doc("invitedBy" -> invitedBy))
  }
}
