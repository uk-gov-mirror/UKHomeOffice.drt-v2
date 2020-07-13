package drt.shared.api

import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.MilliTimes.oneMinuteMillis
import drt.shared.Terminals.Terminal
import drt.shared._
import upickle.default.{ReadWriter, macroRW}

import scala.collection.immutable.NumericRange
import scala.util.matching.Regex

case class FlightCodeSuffix(suffix: String)

case class Arrival(operator: Option[Operator],
                   carrierCode: CarrierCode,
                   voyageNumber: VoyageNumber,
                   flightCodeSuffix: Option[FlightCodeSuffix],
                   status: ArrivalStatus,
                   sstimated: Option[MillisSinceEpoch],
                   actual: Option[MillisSinceEpoch],
                   estimatedChox: Option[MillisSinceEpoch],
                   actualChox: Option[MillisSinceEpoch],
                   gate: Option[String],
                   stand: Option[String],
                   maxPax: Option[Int],
                   actPax: Option[Int],
                   tranPax: Option[Int],
                   runwayID: Option[String],
                   baggageReclaimId: Option[String],
                   airportID: PortCode,
                   terminal: Terminal,
                   origin: PortCode,
                   scheduled: MillisSinceEpoch,
                   pcpTime: Option[MillisSinceEpoch],
                   feedSources: Set[FeedSource],
                   carrierScheduled: Option[MillisSinceEpoch],
                   apiPax: Option[Int]
                  ) extends WithUnique[UniqueArrival] {
  val paxOffPerMinute = 20

  def suffixString: String = flightCodeSuffix match {
    case None => ""
    case Some(s) => s.suffix
  }

  def flightCode: String = s"$carrierCode${voyageNumber.toPaddedString}$suffixString"

  def basicForComparison: Arrival = copy(pcpTime = None)

  def equals(arrival: Arrival): Boolean = arrival.basicForComparison == basicForComparison

  lazy val uniqueId: Int = uniqueStr.hashCode
  lazy val uniqueStr: String = s"$terminal$scheduled${voyageNumber.numeric}"

  def hasPcpDuring(start: SDateLike, end: SDateLike): Boolean = {
    val firstPcpMilli = pcpTime.getOrElse(0L)
    val lastPcpMilli = firstPcpMilli + millisToDisembark(actPax.getOrElse(0))
    val firstInRange = start.millisSinceEpoch <= firstPcpMilli && firstPcpMilli <= end.millisSinceEpoch
    val lastInRange = start.millisSinceEpoch <= lastPcpMilli && lastPcpMilli <= end.millisSinceEpoch
    firstInRange || lastInRange
  }

  def millisToDisembark(pax: Int): Long = {
    val minutesToDisembark = (pax.toDouble / 20).ceil
    val oneMinuteInMillis = 60 * 1000
    (minutesToDisembark * oneMinuteInMillis).toLong
  }

  lazy val pax: Int = actPax.getOrElse(0)

  lazy val minutesOfPaxArrivals: Int =
    if (pax <= 0) 0
    else (pax.toDouble / paxOffPerMinute).ceil.toInt - 1

  def pcpRange(): NumericRange[MillisSinceEpoch] = {
    val pcpStart = pcpTime.getOrElse(0L)
    val pcpEnd = pcpStart + oneMinuteMillis * minutesOfPaxArrivals
    pcpStart to pcpEnd by oneMinuteMillis
  }

  lazy val unique: UniqueArrival = UniqueArrival(voyageNumber.numeric, terminal, scheduled)

  def isCancelled: Boolean = status.description match {
    case st if st.toLowerCase.contains("cancelled") => true
    case st if st.toLowerCase.contains("canceled") => true
    case st if st.toLowerCase.contains("deleted") => true
    case _ => false
  }
}

object Arrival {
  val flightCodeRegex: Regex = "^([A-Z0-9]{2,3}?)([0-9]{1,4})([A-Z]*)$".r

  def summaryString(arrival: Arrival): String = arrival.airportID + "/" + arrival.terminal + "@" + arrival.scheduled + "!" + arrival.flightCode

  def standardiseFlightCode(flightCode: String): String = {
    val flightCodeRegex = "^([A-Z0-9]{2,3}?)([0-9]{1,4})([A-Z]?)$".r

    flightCode match {
      case flightCodeRegex(operator, flightNumber, suffix) =>
        val number = f"${flightNumber.toInt}%04d"
        f"$operator$number$suffix"
      case _ => flightCode
    }
  }

  implicit val arrivalStatusRw: ReadWriter[ArrivalStatus] = macroRW
  implicit val voyageNumberRw: ReadWriter[VoyageNumber] = macroRW
  implicit val arrivalSuffixRw: ReadWriter[FlightCodeSuffix] = macroRW
  implicit val operatorRw: ReadWriter[Operator] = macroRW
  implicit val portCodeRw: ReadWriter[PortCode] = macroRW
  implicit val arrivalRw: ReadWriter[Arrival] = macroRW

  def apply(operator: Option[Operator],
            status: ArrivalStatus,
            estimated: Option[MillisSinceEpoch],
            actual: Option[MillisSinceEpoch],
            estimatedChox: Option[MillisSinceEpoch],
            actualChox: Option[MillisSinceEpoch],
            gate: Option[String],
            stand: Option[String],
            maxPax: Option[Int],
            actPax: Option[Int],
            tranPax: Option[Int],
            runwayID: Option[String],
            baggageReclaimId: Option[String],
            airportID: PortCode,
            terminal: Terminal,
            rawICAO: String,
            rawIATA: String,
            origin: PortCode,
            scheduled: MillisSinceEpoch,
            pcpTime: Option[MillisSinceEpoch],
            feedSources: Set[FeedSource],
            carrierScheduled: Option[MillisSinceEpoch] = None,
            apiPax: Option[Int] = None
           ): Arrival = {
    val (carrierCode: CarrierCode, voyageNumber: VoyageNumber, maybeSuffix: Option[FlightCodeSuffix]) = {
      val bestCode = (rawIATA, rawICAO) match {
        case (iata, _) if iata != "" => iata
        case (_, icao) if icao != "" => icao
        case _ => ""
      }

      FlightParsing.flightCodeToParts(bestCode)
    }

    Arrival(
      operator = operator,
      carrierCode = carrierCode,
      voyageNumber = voyageNumber,
      flightCodeSuffix = maybeSuffix,
      status = status,
      sstimated = estimated,
      actual = actual,
      estimatedChox = estimatedChox,
      actualChox = actualChox,
      gate = gate,
      stand = stand,
      maxPax = maxPax,
      actPax = actPax,
      tranPax = tranPax,
      runwayID = runwayID,
      baggageReclaimId = baggageReclaimId,
      airportID = airportID,
      terminal = terminal,
      origin = origin,
      scheduled = scheduled,
      pcpTime = pcpTime,
      feedSources = feedSources,
      carrierScheduled = carrierScheduled,
      apiPax = apiPax
      )
  }
}
