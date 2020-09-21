package drt.shared

import drt.shared.CrunchApi.MillisSinceEpoch

import scala.util.{Success, Try}

trait DateLike {
  val year: Int
  val month: Int
  val day: Int

  val timeZoneId: String

  val firstMinute: SDateLike
  val lastMinute: SDateLike = firstMinute.addDays(1).addMinutes(-1)

  val firstMinuteMillis: MillisSinceEpoch = firstMinute.millisSinceEpoch
  val lastMinuteMillis: MillisSinceEpoch = lastMinute.millisSinceEpoch

  val toISOString = f"$year-$month%02d-$day%02d"

  override def toString: String = toISOString
}

object DateLike {
  trait UtcDateLike extends DateLike with DateToFirstMinuteLike {
    final override val timeZoneId: String = "UTC"
  }

  def parse(fromYearMonthDay: (Int, Int, Int) => DateLike): String => Option[DateLike] =
    (dateString: String) => Try(
      dateString
        .split("-")
        .take(3)
        .toList
        .map(_.toInt)
    ) match {
      case Success(year :: month :: day :: _) =>
        Option(fromYearMonthDay(year, month, day))
      case _ => None
    }
}

trait DateToFirstMinuteLike {
  val year: Int
  val month: Int
  val day: Int

  val timeZoneId: String

  val firstMinute: SDateLike
}
