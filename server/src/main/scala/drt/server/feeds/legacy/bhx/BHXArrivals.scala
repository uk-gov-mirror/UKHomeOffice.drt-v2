package drt.server.feeds.legacy.bhx

import drt.server.feeds.Implicits._
import drt.shared.Terminals.Terminal
import drt.shared.api.Arrival
import drt.shared.{ForecastFeedSource, LiveFeedSource, PortCode}
import javax.xml.datatype.XMLGregorianCalendar
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import org.springframework.util.StringUtils
import services.SDate
import uk.co.bhx.online.flightinformation.{FlightRecord, ScheduledFlightRecord}

sealed trait BHXArrivals {

  def convertToUTC(feedDate: XMLGregorianCalendar): Option[String] = {
    val date = feedDate.toGregorianCalendar.getTime

    date.getTime match {
      case 0L => None
      case _ =>
        val datetime = new DateTime(date.getTime).withMillisOfSecond(0).withSecondOfMinute(0)
        Some(datetime.withZone(DateTimeZone.UTC).toString(ISODateTimeFormat.dateTime))
    }
  }

  def convertToUTCPlusOneHour(feedDate: XMLGregorianCalendar): String = {
    val utcDatePlusOneHour = new DateTime(feedDate.toGregorianCalendar.getTimeInMillis, DateTimeZone.UTC)
      .plusHours(1)
      .withMillisOfSecond(0)
      .withSecondOfMinute(0)
    utcDatePlusOneHour.toString(ISODateTimeFormat.dateTime)
  }

}

trait BHXLiveArrivals extends BHXArrivals {

  def toLiveArrival(flightRecord: FlightRecord): Arrival = {
    val actPax = flightRecord.getPassengers
    val transPax = flightRecord.getTransits

    Arrival(
      operator = None,
      status = flightRecord.getFlightStatus,
      estimated = convertToUTC(flightRecord.getEstimatedTime).map(SDate(_).millisSinceEpoch),
      actual = convertToUTC(flightRecord.getTouchdownTime).map(SDate(_).millisSinceEpoch),
      estimatedChox = convertToUTC(flightRecord.getEstimatedChoxTime).map(SDate(_).millisSinceEpoch),
      actualChox = convertToUTC(flightRecord.getChoxTime).map(SDate(_).millisSinceEpoch),
      gate = if (StringUtils.isEmpty(flightRecord.getGate)) None else Option(flightRecord.getGate),
      stand = if (StringUtils.isEmpty(flightRecord.getStand)) None else Option(flightRecord.getStand),
      maxPax = if (flightRecord.getCapacity == 0) None else Option(flightRecord.getCapacity),
      actPax = if (actPax == 0) None else Option(actPax),
      tranPax = if (actPax == 0) None else Option(transPax),
      runwayID = if (StringUtils.isEmpty(flightRecord.getRunway)) None else Option(flightRecord.getRunway),
      baggageReclaimId = Option(flightRecord.getBelt),
      airportID = "BHX",
      terminal = Terminal(s"T${flightRecord.getTerminal}"),
      rawICAO = flightRecord.getFlightNumber,
      rawIATA = flightRecord.getFlightNumber,
      origin = PortCode(flightRecord.getOrigin),
      scheduled = convertToUTC(flightRecord.getScheduledTime).map(SDate(_).millisSinceEpoch).getOrElse(0),
      pcpTime = None,
      feedSources = Set(LiveFeedSource)
    )
  }
}

trait BHXForecastArrivals extends BHXArrivals {

  def toForecastArrival(flightRecord: ScheduledFlightRecord): Arrival = {
    val maxPax = flightRecord.getCapacity
    val actPax = flightRecord.getPassengers
    val transPax = flightRecord.getTransits
    Arrival(
      operator = None,
      status = "Port Forecast",
      estimated = None,
      actual = None,
      estimatedChox = None,
      actualChox = None,
      gate = None,
      stand = None,
      maxPax = if (maxPax == 0) None else Option(maxPax),
      actPax = if (actPax == 0) None else Option(actPax),
      tranPax = if (actPax == 0) None else Option(transPax),
      runwayID = None,
      baggageReclaimId = None,
      airportID = "BHX",
      terminal = Terminal(s"T${flightRecord.getTerminal}"),
      rawICAO = flightRecord.getFlightNumber,
      rawIATA = flightRecord.getFlightNumber,
      origin = flightRecord.getOrigin,
      scheduled = SDate(convertToUTCPlusOneHour(flightRecord.getScheduledTime)).millisSinceEpoch,
      pcpTime = None,
      feedSources = Set(ForecastFeedSource)
    )
  }
}
