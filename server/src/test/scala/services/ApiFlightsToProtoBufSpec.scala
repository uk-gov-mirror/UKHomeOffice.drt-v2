package services

import actors.FlightMessageConversion._
import drt.shared.Terminals.T2
import drt.shared.api.Arrival
import drt.shared.{ApiFeedSource, ArrivalStatus, Operator, PortCode}
import org.specs2.mutable.Specification
import server.protobuf.messages.FlightsMessage.FlightMessage

class ApiFlightsToProtoBufSpec extends Specification {

  "apiFlightToFlightMessage" should {
    "take a single Arrival and return a FlightMessage representing it" in {
      val apiFlight = Arrival(
        operator = Option(Operator("Op")),
        status = ArrivalStatus("scheduled"),
        estimated = Option(SDate("2016-01-01T13:05:00Z").millisSinceEpoch),
        actual = Option(SDate("2016-01-01T13:10:00Z").millisSinceEpoch),
        estimatedChox = Option(SDate("2016-01-01T13:15:00Z").millisSinceEpoch),
        actualChox = Option(SDate("2016-01-01T13:20:00Z").millisSinceEpoch),
        gate = Option("10"),
        stand = Option("10A"),
        maxPax = Option(200),
        actPax = Option(150),
        tranPax = Option(10),
        runwayID = Option("1"),
        baggageReclaimId = Option("A"),
        airportID = PortCode("LHR"),
        terminal = T2,
        rawICAO = "BAA0001",
        rawIATA = "BA0001",
        origin = PortCode("JFK"),
        pcpTime = Option(1451655000000L), // 2016-01-01 13:30:00 UTC
        scheduled = SDate("2016-01-01T13:00:00Z").millisSinceEpoch,
        feedSources = Set(ApiFeedSource),
        carrierScheduled = Option(100L)
      )
      val flightMessage = apiFlightToFlightMessage(apiFlight)

      val expected = FlightMessage(
        operator = Option("Op"),
        gate = Option("10"),
        stand = Option("10A"),
        status = Option("scheduled"),
        maxPax = Option(200),
        actPax = Option(150),
        tranPax = Option(10),
        runwayID = Option("1"),
        baggageReclaimId = Option("A"),
        airportID = Option("LHR"),
        terminal = Option("T2"),
        iCAO = Option("BA0001"),
        iATA = Option("BA0001"),
        origin = Option("JFK"),
        feedSources = Seq("ApiFeedSource"),
        pcpTime = Option(1451655000000L), // 2016-01-01 13:30:00 UTC
        scheduled = Option(1451653200000L), // 2016-01-01 13:00:00 UTC
        estimated = Option(1451653500000L), // 2016-01-01 13:05:00 UTC
        touchdown = Option(1451653800000L), // 2016-01-01 13:10:00 UTC
        estimatedChox = Option(1451654100000L), // 2016-01-01 13:15:00 UTC
        actualChox = Option(1451654400000L), // 2016-01-01 13:20:00 UTC
        carrierScheduled = Option(100L)
      )

      flightMessage === expected
    }
  }
}
