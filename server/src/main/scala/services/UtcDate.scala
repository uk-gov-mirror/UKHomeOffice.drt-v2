package services

import drt.shared.DateLike.UtcDateLike
import drt.shared.{DateLike, DateToFirstMinuteLike, SDateLike}
import org.joda.time.DateTimeZone
import services.graphstages.Crunch

trait DateToFirstMinute extends DateToFirstMinuteLike {
  final lazy val firstMinute: SDateLike = SDate(year, month, day, DateTimeZone.forID(timeZoneId))
}

case class UtcDate(year: Int, month: Int, day: Int) extends UtcDateLike with DateToFirstMinute

object UtcDate {
  def apply(date: SDateLike): UtcDate = {
    val utcSDate = SDate(date.millisSinceEpoch, Crunch.utcTimeZone)
    UtcDate(utcSDate.getFullYear(), utcSDate.getMonth(), utcSDate.getDate())
  }

  private val fromYearMonthDay = (year: Int, month: Int, day: Int) => UtcDate(year, month, day)

  def apply(date: String): String => Option[DateLike] = DateLike.parse(fromYearMonthDay)
}
