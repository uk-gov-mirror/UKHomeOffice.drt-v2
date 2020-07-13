package actors

import drt.shared.Terminals.T1
import drt.shared.api.{Arrival, FlightCodeSuffix}
import drt.shared.{AclFeedSource, ApiFeedSource, FeedSource, LiveFeedSource, Operator, PortCode}
import org.specs2.mutable.Specification
import server.protobuf.messages.FlightsMessage.FlightMessage

class FlightMessageConversionSpec extends Specification {

  import drt.server.feeds.Implicits._

  val arrival = Arrival(
    operator = Option(Operator("BA")),
    status = "landed",
    estimated = Option(2L),
    actual = Option(3L),
    estimatedChox = Option(4L),
    actualChox = Option(5L),
    gate = Option("G1"),
    stand = Option("S1"),
    maxPax = Option(350),
    actPax = Option(122),
    tranPax = Option(10),
    runwayID = Option("R1"),
    baggageReclaimId = Option("B1"),
    airportID = PortCode("LHR"),
    terminal = T1,
    rawICAO = "BAA1111",
    rawIATA = "BA1111",
    origin = PortCode("JFK"),
    scheduled = 1L,
    pcpTime = Option(10L),
    feedSources = Set(AclFeedSource, LiveFeedSource),
    carrierScheduled = Option(4L)
    )

  "Given an Arrival with no suffix" >> {
    "When I convert it to a protobuf message and then back to an Arrival" >> {
      val arrivalMessage = FlightMessageConversion.apiFlightToFlightMessage(arrival)
      val restoredArrival = FlightMessageConversion.flightMessageToApiFlight(arrivalMessage)
      "Then the converted Arrival should match the original" >> {
        restoredArrival === arrival
      }
    }
  }

  "Given an Arrival with a suffix" >> {
    val arrivalWithSuffix = arrival.copy(flightCodeSuffix = Option(FlightCodeSuffix("P")))
    "When I convert it to a protobuf message and then back to an Arrival" >> {
      val arrivalMessage = FlightMessageConversion.apiFlightToFlightMessage(arrivalWithSuffix)
      val restoredArrival = FlightMessageConversion.flightMessageToApiFlight(arrivalMessage)
      "Then the converted Arrival should match the original" >> {
        restoredArrival === arrivalWithSuffix
      }
    }
  }

  "Given an arrival with 0 Passengers" >> {
    val arrivalWith0Pax = arrival.copy(actPax = Option(0), tranPax = Option(0), maxPax = Option(0))
    "When I convert it to a protobuf message and then back to an Arrival" >> {
      val arrivalMessage = FlightMessageConversion.apiFlightToFlightMessage(arrivalWith0Pax)
      val restoredArrival = FlightMessageConversion.flightMessageToApiFlight(arrivalMessage)
      "Then the converted Arrival should match the original" >> {
        restoredArrival === arrivalWith0Pax
      }
    }
  }
}
