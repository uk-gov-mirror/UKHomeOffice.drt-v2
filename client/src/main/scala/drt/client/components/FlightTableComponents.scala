package drt.client.components

import drt.client.services.JSDateConversions.SDate
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared._
import drt.shared.api.Arrival
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.{TagMod, TagOf}
import org.scalajs.dom.html.Div

object FlightTableComponents {

  def localDateTimeWithPopup(dt: Option[MillisSinceEpoch]): TagMod = {
    dt.map(millis => localTimePopup(millis)).getOrElse(<.span())
  }

  def localTimePopup(dt: MillisSinceEpoch): VdomElement = {
    sdateLocalTimePopup(SDate(dt))
  }

  def millisToDisembark(pax: Int): Long = {
    val minutesToDisembark = (pax.toDouble / 20).ceil
    val oneMinuteInMillis = 60 * 1000
    (minutesToDisembark * oneMinuteInMillis).toLong
  }

  def pcpTimeRange(arrival: Arrival): TagOf[Div] =
    arrival.PcpTime.map { pcpTime: MillisSinceEpoch =>
      val sdateFrom = SDate(MilliDate(pcpTime))
      val sdateTo = SDate(MilliDate(pcpTime + millisToDisembark(arrival.bestPaxEstimate)))
      <.div(
        sdateLocalTimePopup(sdateFrom),
        " \u2192 ",
        sdateLocalTimePopup(sdateTo)
      )
    } getOrElse {
      <.div()
    }

  def sdateLocalTimePopup(sdate: SDateLike): Unmounted[Tippy.Props, Unit, Unit] = {
    val hhmm = f"${sdate.getHours()}%02d:${sdate.getMinutes()}%02d"
    Tippy.describe(<.span(sdate.toLocalDateTimeString()), hhmm)
  }

  val uniqueArrivalsWithCodeShares: Seq[ApiFlightWithSplits] => List[(ApiFlightWithSplits, Set[Arrival])] = CodeShares.uniqueArrivalsWithCodeShares((f: ApiFlightWithSplits) => identity(f.apiFlight))
}
