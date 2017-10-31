package lila.user

import org.joda.time.DateTime

case class Trophy(
    _id: String, // random
    user: String,
    kind: Trophy.Kind,
    date: DateTime
) extends Ordered[Trophy] {

  def timestamp = date.getMillis

  def compare(other: Trophy) =
    if (kind.order == other.kind.order) timestamp compare other.timestamp
    else kind.order compare other.kind.order
}

object Trophy {

  sealed abstract class Kind(
    val key: String,
    val name: String,
    val icon: Option[String],
    val url: Option[String],
    val klass: Option[String],
    val order: Int
  )

  object Kind {

    object ZugMiracle extends Kind(
      key = "zugMiracle",
      name = "Zug miracle",
      icon = none,
      url = "//lichess.org/qa/259/how-do-you-get-a-zug-miracle-trophy".some,
      klass = none,
      order = 1
    )

    object WayOfBerserk extends Kind(
      key = "wayOfBerserk",
      name = "The way of Berserk",
      icon = "`".some,
      url = "//lichess.org/qa/340/way-of-berserk-trophy".some,
      klass = "fire_trophy".some,
      order = 2
    )

    object MarathonWinner extends Kind(
      key = "marathonWinner",
      name = "Marathon Winner",
      icon = "\\".some,
      url = none,
      klass = "fire_trophy".some,
      order = 3
    )

    object MarathonTopTen extends Kind(
      key = "marathonTopTen",
      name = "Marathon Top 10",
      icon = "\\".some,
      url = none,
      klass = "fire_trophy".some,
      order = 4
    )

    object MarathonTopFifty extends Kind(
      key = "marathonTopFifty",
      name = "Marathon Top 50",
      icon = "\\".some,
      url = none,
      klass = "fire_trophy".some,
      order = 5
    )

    object MarathonTopHundred extends Kind(
      key = "marathonTopHundred",
      name = "Marathon Top 100",
      icon = "\\".some,
      url = none,
      klass = "fire_trophy".some,
      order = 6
    )

    object MarathonSurvivor extends Kind(
      key = "marathonSurvivor",
      name = "Marathon #1 survivor",
      icon = ",".some,
      url = "//lichess.org/blog/VXF45yYAAPQgLH4d/chess-marathon-1".some,
      klass = "fire_trophy".some,
      order = 7
    )

    object BongcloudWarrior extends Kind(
      key = "bongcloudWarrior",
      name = "Bongcloud Warrior",
      icon = "~".some,
      url = "//lichess.org/forum/lichess-feedback/bongcloud-trophy".some,
      klass = "fire_trophy".some,
      order = 8
    )

    object Developer extends Kind(
      key = "developer",
      name = "Lichess developer",
      icon = "&#xe000;".some,
      url = "https://github.com/ornicar/lila/graphs/contributors".some,
      klass = "icon3d".some,
      order = 100
    )

    object Moderator extends Kind(
      key = "moderator",
      name = "Lichess moderator",
      icon = "&#xe002;".some,
      url = "//lichess.org/report".some,
      "icon3d".some,
      order = 101
    )

    object Streamer extends Kind(
      key = "streamer",
      name = "Lichess streamer",
      icon = "&#xe003;".some,
      url = "//lichess.org/help/stream-on-lichess".some,
      "icon3d".some,
      order = 102
    )

    object HalloweeneeneenLeft extends Kind(
      key = "halloweeneeneenleft",
      name = "Halloweeneeneen Tournament Champion",
      icon = none,
      url = "//bughousetest.com/tournament/0L8mg5NK".some,
      none,
      order = 103
    )

    object HalloweeneeneenRight extends Kind(
      key = "halloweeneeneenright",
      name = "Halloweeneeneen Tournament Champion",
      icon = none,
      url = "//bughousetest.com/tournament/0L8mg5NK".some,
      none,
      order = 104
    )

    object HalloweeneeneenLeft2 extends Kind(
      key = "halloweeneeneenleft2",
      name = "Halloweeneeneen Tournament Runner-up",
      icon = none,
      url = "//bughousetest.com/tournament/0L8mg5NK".some,
      none,
      order = 105
    )

    object HalloweeneeneenRight2 extends Kind(
      key = "halloweeneeneenright2",
      name = "Halloweeneeneen Tournament Runner-up",
      icon = none,
      url = "//bughousetest.com/tournament/0L8mg5NK".some,
      none,
      order = 106
    )

    val all = List(
      ZugMiracle,
      WayOfBerserk,
      MarathonSurvivor,
      MarathonWinner, MarathonTopTen, MarathonTopFifty, MarathonTopHundred,
      BongcloudWarrior,
      Developer, Moderator,
      Streamer,
      HalloweeneeneenLeft, HalloweeneeneenRight, HalloweeneeneenLeft2, HalloweeneeneenRight2
    )
    def byKey(key: String) = all find (_.key == key)
  }

  def make(userId: String, kind: Trophy.Kind) = Trophy(
    _id = ornicar.scalalib.Random nextString 8,
    user = userId,
    kind = kind,
    date = DateTime.now
  )
}
