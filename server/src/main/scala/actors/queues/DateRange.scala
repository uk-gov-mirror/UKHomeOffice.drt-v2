package actors.queues

import akka.NotUsed
import akka.stream.scaladsl.Source
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.dates.{DateLike, LocalDate, UtcDate}
import drt.shared.{MilliTimes, SDateLike}
import org.checkerframework.checker.units.qual.m
import services.SDate

object DateRange {
  type MillisToDateLike[A <: DateLike] = MillisSinceEpoch => A

  val millisToUtc: MillisToDateLike[UtcDate] = (millis: MillisSinceEpoch) => SDate(millis).toUtcDate
  val millisToLocal: MillisToDateLike[LocalDate] = (millis: MillisSinceEpoch) => SDate(millis).toLocalDate

  def dateRangeWithBuffer[A <: DateLike](startBuffer: Int, endBuffer: Int, millisToDate: MillisToDateLike[A])(start: SDateLike, end: SDateLike): Seq[A] = {
    val lookupStartMillis = start.addDays(startBuffer * -1).millisSinceEpoch
    val lookupEndMillis = end.addDays(endBuffer).millisSinceEpoch
    val daysRangeMillis = lookupStartMillis to lookupEndMillis by MilliTimes.oneDayMillis
    daysRangeMillis.map(millisToDate)
  }

  def dateRange[A <: DateLike](start: SDateLike, end: SDateLike, millisToDate: MillisToDateLike[A]): Seq[A] = {
    val lookupStartMillis = start.millisSinceEpoch
    val lookupEndMillis = end.millisSinceEpoch
    val daysRangeMillis = (lookupStartMillis to lookupEndMillis by MilliTimes.oneDayMillis) :+ end.millisSinceEpoch
    daysRangeMillis.map(millisToDate).distinct
  }

  def dateRangeSource[A <: DateLike](start: SDateLike, end: SDateLike, millisToDate: MillisToDateLike[A]): Source[A, NotUsed] =
    Source(dateRange(start, end, millisToDate).toList)

  def utcDateRangeWithBuffer(startBuffer: Int, endBuffer: Int)(start: SDateLike, end: SDateLike): Seq[UtcDate] =
    dateRangeWithBuffer(startBuffer, endBuffer, millisToUtc)(start, end)

  def localDateRangeWithBuffer(startBuffer: Int, endBuffer: Int)(start: SDateLike, end: SDateLike): Seq[LocalDate] =
    dateRangeWithBuffer(startBuffer, endBuffer, millisToLocal)(start, end)

  def utcDateRange(start: SDateLike, end: SDateLike): Seq[UtcDate] =
    dateRange(start, end, millisToUtc)

  def localDateRange(start: SDateLike, end: SDateLike): Seq[LocalDate] =
    dateRange(start, end, millisToLocal)

  def utcDateRangeSource(start: SDateLike, end: SDateLike): Source[UtcDate, NotUsed] =
    Source(dateRange(start, end, millisToUtc).toList)

  def localDateRangeSource(start: SDateLike, end: SDateLike): Source[LocalDate, NotUsed] =
    Source(dateRange(start, end, millisToLocal).toList)
}
