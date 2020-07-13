package drt.server.feeds.lgw

import drt.server.feeds.Implicits._
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.Terminals.{InvalidTerminal, N, S}
import drt.shared._
import drt.shared.api.Arrival
import org.apache.commons.lang3.StringUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import scala.xml.Node

case class ResponseToArrivals(data: String) {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def getArrivals: List[Arrival] = Try {
    scala.xml.Utility.trimProper(
      scala.xml.XML.loadString(data)
    )
      .map(nodeToArrival)

  } match {
    case Success(arrivalsAndLocation) => arrivalsAndLocation.toList
    case Failure(t) =>
      log.error(s"Failed to get an Arrival from the Gatwick XML.", t)
      List.empty[Arrival]
  }

  def nodeToArrival: Node => Arrival = (n: Node) => {

    val operator = (n \ "AirlineIATA") text
    val actPax = parsePaxCount(n, "70A").orElse(None)
    val transPax = parsePaxCount(n, "TIP")
    val arrival = new Arrival(
      operator = if (operator.isEmpty) None else Option(Operator(operator)),
      status = parseStatus(n),
      sstimated = parseDateTime(n, operationQualifier = "TDN", timeType = "EST"),
      actual = parseDateTime(n, operationQualifier = "TDN", timeType = "ACT"),
      estimatedChox = parseDateTime(n, operationQualifier = "ONB", timeType = "EST"),
      actualChox = parseDateTime(n, operationQualifier = "ONB", timeType = "ACT"),
      gate = (n \\ "PassengerGate").headOption.map(n => n text).filter(StringUtils.isNotBlank(_)),
      stand = (n \\ "ArrivalStand").headOption.map(n => n text).filter(StringUtils.isNotBlank(_)),
      maxPax = (n \\ "SeatCapacity").headOption.map(n => (n text).toInt),
      actPax = actPax,
      tranPax = if (actPax.isEmpty) None else transPax,
      runwayID = parseRunwayId(n).filter(StringUtils.isNotBlank(_)),
      baggageReclaimId = Try(n \\ "BaggageClaimUnit" text).toOption.filter(StringUtils.isNotBlank(_)),
      airportID = "LGW",
      terminal = parseTerminal(n),
      carrierCode = CarrierCode((n \\ "AirlineIATA" text)),
      voyageNumber = VoyageNumber(parseFlightNumber(n)),
      flightCodeSuffix = None,
      origin = parseOrigin(n),
      scheduled = (((n \ "FlightLeg").head \ "LegData").head \\ "OperationTime").find(n => (n \ "@OperationQualifier" text).equals("ONB") && (n \ "@TimeType" text).equals("SCT")).map(n => services.SDate.parseString(n text).millisSinceEpoch).getOrElse(0),
      pcpTime = None,
      feedSources = Set(LiveFeedSource),
      carrierScheduled = None,
      apiPax = None
      )
    log.debug(s"parsed arrival: $arrival")
    arrival
  }

  private def parseTerminal(n: Node): Terminals.Terminal = {
    val terminal = (n \\ "AirportResources" \ "Resource").find(n => (n \ "@DepartureOrArrival" text).equals("Arrival")).map(n => n \\ "AircraftTerminal" text).getOrElse("")
    val mappedTerminal = terminal match {
      case "1" => S
      case "2" => N
      case _ => InvalidTerminal
    }
    mappedTerminal
  }

  private def parseFlightNumber(n: Node): Int = {
    (((n \ "FlightLeg").head \ "LegIdentifier").head \ "FlightNumber" text).toInt
  }

  def parseStatus(n: Node): String = {
    val aidxCodeOrIdahoCode = ((n \ "FlightLeg").head \ "LegData").head \ "OperationalStatus" text

    aidxCodeOrIdahoCode match {
      case "DV" => "Diverted"
      case "DX" | "CX" => "Cancelled"
      case "EST" | "ES" => "Estimated"
      case "EXP" | "EX" => "Expected"
      case "FRB" | "FB" => "First Bag Delivered"
      case "LAN" | "LD" => "Landed"
      case "LSB" | "LB" => "Last Bag Delivered"
      case "NIT" | "NI" => "Next Information Time"
      case "ONB" | "OC" => "On Chocks"
      case "OVS" | "OV" => "Overshoot"
      case "REM" | "**" => "Deleted / Removed Flight Record"
      case "SCT" | "SH" => "Scheduled"
      case "TEN" | "FS" => "Final Approach"
      case "THM" | "ZN" => "Zoning"
      case "UNK" | "??" => "Unknown"
      case "FCT" | "LC" => "Last Call (Departure Only)"
      case "BST" | "BD" => "Boarding (Departure Only)"
      case "GCL" | "GC" => "Gate Closed (Departure Only)"
      case "GOP" | "GO" => "Gate Opened (Departure Only)"
      case "RST" | "RS" => "Return to Stand (Departure Only)"
      case "OFB" | "TX" => "Taxied (Departure Only)"
      case "TKO" | "AB" => "Airborne (Departure Only)"
      case unknownCode => unknownCode
    }
  }

  def parseOrigin(n: Node): String = {
    ((n \ "FlightLeg").head \ "LegIdentifier").head \ "DepartureAirport" text
  }

  def parseRunwayId(n: Node): Option[String] = {
    (n \\ "AirportResources" \ "Resource").find(n => (n \ "@DepartureOrArrival" text).equals("Arrival")).map(n => n \\ "Runway" text)
  }

  def parsePaxCount(n: Node, qualifier: String): Option[Int] = {
    (n \\ "CabinClass").find(n => (n \ "@Class").isEmpty).flatMap(n => (n \ "PaxCount").find(n => (n \ "@Qualifier" text).equals(qualifier)).map(n => (n text).toInt))
  }

  def parseDateTime(n: Node, operationQualifier: String, timeType: String): Option[MillisSinceEpoch] = {
    (((n \ "FlightLeg").head \ "LegData").head \\ "OperationTime").find(n => (n \ "@OperationQualifier" text).equals(operationQualifier) && (n \ "@TimeType" text).equals(timeType)).map(n => services.SDate.parseString(n text).millisSinceEpoch)
  }

}
