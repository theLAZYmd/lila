package lila.tournament

import akka.actor._
import akka.pattern.pipe
import org.joda.time.DateTime
import org.joda.time.DateTimeConstants._
import scala.concurrent.duration._

import actorApi._
import chess.StartingPosition

private final class TournamentScheduler private (api: TournamentApi) extends Actor {

  import Schedule.Freq._
  import Schedule.Speed._
  import chess.variant._

  // def marathonDates = List(
  // Spring -> Saturday of the weekend after Orthodox Easter Sunday
  // Summer -> first Saturday of August
  // Autumn -> Saturday of weekend before the weekend Halloween falls on (c.f. half-term holidays)
  // Winter -> 28 December, convenient day in the space between Boxing Day and New Year's Day
  // Summer -> day(2015, 8, 1),
  // Autumn -> day(2015, 10, 24),
  // Winter -> day(2015, 12, 28),
  // Spring -> day(2016, 4, 16),
  // Summer -> day(2016, 8, 6),
  // Autumn -> day(2016, 10, 22),
  // Winter -> day(2016, 12, 28)
  // )

  def receive = {

    case ScheduleNow =>
      TournamentRepo.scheduledUnfinished.map(_.flatMap(_.schedule)) map
        ScheduleNowWith.apply pipeTo self

    case ScheduleNowWith(dbScheds) => try {

      val rightNow = DateTime.now
      val today = rightNow.withTimeAtStartOfDay
      val tomorrow = rightNow plusDays 1
      val startOfYear = today.dayOfYear.withMinimumValue

      val lastDayOfMonth = today.dayOfMonth.withMaximumValue

      val lastWeekOfMonth = lastDayOfMonth.minusDays((lastDayOfMonth.getDayOfWeek - 1) % 7)

      def nextDayOfWeek(number: Int) = today.plusDays((number + 7 - today.getDayOfWeek) % 7)
      val nextMonday = nextDayOfWeek(1)
      val nextTuesday = nextDayOfWeek(2)
      val nextWednesday = nextDayOfWeek(3)
      val nextThursday = nextDayOfWeek(4)
      val nextFriday = nextDayOfWeek(5)
      val nextSaturday = nextDayOfWeek(6)
      val nextSunday = nextDayOfWeek(7)

      def secondWeekOf(month: Int) = {
        val start = orNextYear(startOfYear.withMonthOfYear(month))
        start.plusDays(15 - start.getDayOfWeek)
      }

      def orTomorrow(date: DateTime) = if (date isBefore rightNow) date plusDays 1 else date
      def orNextWeek(date: DateTime) = if (date isBefore rightNow) date plusWeeks 1 else date
      def orNextYear(date: DateTime) = if (date isBefore rightNow) date plusYears 1 else date

      val isHalloween = today.getDayOfMonth == 31 && today.getMonthOfYear == OCTOBER

      val std = StartingPosition.initial
      def opening(offset: Int) = {
        val positions = StartingPosition.featurable
        positions((today.getDayOfYear + offset) % positions.size)
      }

      val farFuture = today plusMonths 5

      // all dates UTC
      val nextSchedules: List[Schedule] = List(
        // hourly bughouse tournaments!
        (0 to 6).toList.flatMap { hourDelta =>
          val date = rightNow plusHours hourDelta
          val hour = date.getHourOfDay
          val speed = hour % 6 match {
            case 0 | 3 => Bullet
            case 1 | 4 => SuperBlitz
            case 5 => HippoBullet
            case _ => Blitz
          }
          List(
            at(date, hour) map { date => Schedule(Hourly, speed, Bughouse, std, date) },
            at(date, hour, 30) collect {
              case date if speed == Bullet =>
                Schedule(Hourly, if (hour == 18) HyperBullet else Bullet, Bughouse, std, date)
            }
          ).flatten
        }
      ).flatten

      nextSchedules.map { sched =>
        sched.copy(conditions = Schedule conditionFor sched)
      }.foldLeft(List[Schedule]()) {
        case (scheds, sched) if sched.at.isBeforeNow => scheds
        case (scheds, sched) if overlaps(sched, dbScheds) => scheds
        case (scheds, sched) if overlaps(sched, scheds) => scheds
        case (scheds, sched) => sched :: scheds
      } foreach api.createScheduled
    }
    catch {
      case e: org.joda.time.IllegalInstantException =>
        logger.error(s"failed to schedule all: ${e.getMessage}")
    }
  }

  private case class ScheduleNowWith(dbScheds: List[Schedule])

  private def endsAt(s: Schedule) = s.at plus ((~Schedule.durationFor(s)).toLong * 60 * 1000)
  private def interval(s: Schedule) = new org.joda.time.Interval(s.at, endsAt(s))
  private def overlaps(s: Schedule, ss: Seq[Schedule]) = ss exists {
    // prevent daily && weekly on the same day
    case s2 if s.freq.isDailyOrBetter && s2.freq.isDailyOrBetter && s.sameVariantAndSpeed(s2) => s sameDay s2
    // overlapping same variant
    case s2 if s.variant.exotic && s.sameVariant(s2) => interval(s) overlaps interval(s2)
    // overlapping same rating limit
    case s2 if s2.hasMaxRating && s.sameMaxRating(s2) => interval(s) overlaps interval(s2)
    // overlapping similar
    case s2 if s.similarSpeed(s2) && s.sameVariant(s2) && s.sameMaxRating(s2) => interval(s) overlaps interval(s2)
    case _ => false
  }

  private def at(day: DateTime, hour: Int, minute: Int = 0): Option[DateTime] = try {
    Some(day withHourOfDay hour withMinuteOfHour minute withSecondOfMinute 0 withMillisOfSecond 0)
  }
  catch {
    case e: Exception =>
      logger.error(s"failed to schedule one: ${e.getMessage}")
      None
  }
}

private object TournamentScheduler {

  def start(system: ActorSystem, api: TournamentApi) = {
    val ref = system.actorOf(Props(new TournamentScheduler(api)))
    system.scheduler.schedule(1 minute, 5 minutes, ref, ScheduleNow)
  }
}
