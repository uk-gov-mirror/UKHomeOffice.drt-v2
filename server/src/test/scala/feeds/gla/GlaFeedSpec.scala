package feeds.gla

import actors.acking.AckingReceiver.StreamCompleted
import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.stream.scaladsl.Sink
import akka.testkit.TestProbe
import drt.server.feeds.gla.{GlaFeed, GlaFeedRequesterLike, ProdGlaFeedRequester}
import drt.shared.FlightsApi.Flights
import drt.shared.Terminals.T1
import drt.shared.api.Arrival
import drt.shared.{ArrivalStatus, LiveFeedSource, PortCode}
import server.feeds.{ArrivalsFeedFailure, ArrivalsFeedSuccess}
import services.SDate
import services.crunch.CrunchTestLike

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}

case class MockFeedRequester(json: String = "[]") extends GlaFeedRequesterLike {

  var mockResponse: HttpResponse = HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, json))

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  def send(request: HttpRequest)(implicit actorSystem: ActorSystem): Future[HttpResponse] = Future(mockResponse)
}

case class MockExceptionThrowingFeedRequester() extends GlaFeedRequesterLike {

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  def send(request: HttpRequest)(implicit actorSystem: ActorSystem): Future[HttpResponse] = {

    Future(throw new Exception("Something Broke"))
  }
}

class GlaFeedSpec extends CrunchTestLike {
  "Given a GLA Feed I should be able to connect to it and get arrivals back" >> {
    skipped(s"Exploratory test.")
    val prodFeed = GlaFeed(
      uri = sys.env.getOrElse("GLA_LIVE_URL", ""),
      token = sys.env.getOrElse("GLA_LIVE_TOKEN", ""),
      password = sys.env.getOrElse("GLA_LIVE_PASSWORD", ""),
      username = sys.env.getOrElse("GLA_LIVE_USERNAME", ""),
      feedRequester = ProdGlaFeedRequester
    )

    prodFeed.tickingSource.map {
      case ArrivalsFeedSuccess(arrivals, _) =>
        println(s"$arrivals")
      case f: ArrivalsFeedFailure =>
        println(f)
    }.runWith(Sink.seq)

    Thread.sleep(20000)
    true
  }

  def mockFeedWithResponse(res: String) = GlaFeed(
    uri = "http://test.com",
    token = "",
    password = "",
    username = "",
    feedRequester = MockFeedRequester(res)
  )

  "Given a mock json response containing a single valid flight " +
    "I should get a stream with that flight in it " >> {
    val mockFeed = mockFeedWithResponse(firstJsonExample)

    val probe = TestProbe()

    mockFeed.tickingSource.to(Sink.actorRef(probe.ref, StreamCompleted)).run()

    probe.fishForMessage(1 seconds) {
      case s: ArrivalsFeedSuccess if s.arrivals.flights.head.scheduled == SDate("2019-11-13T12:34:00Z").millisSinceEpoch => true
      case _ => false
    }

    success
  }

  "Given a mock json response containing feed with an arrival and a departure " +
    "I should only get the arrival in the end result " >> {
    val dsd = "2019-11-13T17:34:00+00:00"
    val mockFeed = mockFeedWithResponse(containingADepartureJson(dsd))

    val probe = TestProbe()

    mockFeed.tickingSource.to(Sink.actorRef(probe.ref, StreamCompleted)).run()

    probe.fishForMessage(1 seconds) {
      case ArrivalsFeedSuccess(Flights(a), _) if a.size == 1 && !a.exists(_.scheduled == SDate(dsd).millisSinceEpoch) => true
      case _ => false
    }

    success
  }

  "Given a mock json response containing invalid json " +
    "I should get an ArrivalsFeedFailure" >> {
    val mockFeed = mockFeedWithResponse("bad json")

    val result = Await.result(mockFeed.requestArrivals(), 1 second)

    result must haveClass[ArrivalsFeedFailure]
  }

  "Given a feed connection failure then I should get back an ArrivalsFeedFailure." >> {
    val mockFeed = GlaFeed(
      uri = "http://test.com",
      token = "",
      password = "",
      username = "",
      feedRequester = MockExceptionThrowingFeedRequester())

    val result = Await.result(mockFeed.requestArrivals(), 1 second)

    result must haveClass[ArrivalsFeedFailure]
  }

  "Given some valid GLA Feed Json I should get back a valid Arrival object" >> {
    val mockFeed = mockFeedWithResponse(firstJsonExample)

    val expected = Arrival(
      operator = None,
      status = ArrivalStatus("Flight is on schedule"),
      estimated = Some(SDate("2019-11-13T13:32:00Z").millisSinceEpoch),
      actual = Some(SDate("2019-11-13T13:31:00Z").millisSinceEpoch),
      estimatedChox = Some(SDate("2019-11-13T12:33:00Z").millisSinceEpoch),
      actualChox = Some(SDate("2019-11-13T13:30:00Z").millisSinceEpoch),
      gate = Some("G"),
      stand = Some("ST"),
      maxPax = Some(50),
      actPax = Some(20),
      tranPax = None,
      runwayID = Some("3"),
      baggageReclaimId = Some("2"),
      airportID = PortCode("GLA"),
      terminal = T1,
      rawICAO = "TST234",
      rawIATA = "TS234",
      origin = PortCode("TST"),
      scheduled = SDate("2019-11-13T12:34:00Z").millisSinceEpoch,
      pcpTime = None,
      feedSources = Set(LiveFeedSource),
      carrierScheduled = None
    )

    Await.result(mockFeed.requestArrivals(), 1 second) match {
      case ArrivalsFeedSuccess(Flights(arrival :: Nil), _) => arrival === expected
    }
  }

  def firstJsonExample: String =
    """[{
      |        "AIBT": "2019-11-13T13:30:00+00:00",
      |        "AirlineIATA": "TS",
      |        "AirlineICAO": "TST",
      |        "ALDT": "2019-11-13T13:31:00+00:00",
      |        "AODBProbableDateTime": "2019-11-13T13:32:00+00:00",
      |        "CarouselCode": "2",
      |        "CodeShareFlights": "",
      |        "CodeShareInd": "N",
      |        "DepartureArrivalType": "A",
      |        "EIBT": "2019-11-13T12:33:00+00:00",
      |        "FlightNumber": "234",
      |        "FlightStatus": "S",
      |        "FlightStatusDesc": "Flight is on schedule",
      |        "GateCode": "G",
      |        "MaxPax": 50,
      |        "OriginDestAirportIATA": "TST",
      |        "OriginDestAirportICAO": "TSTT",
      |        "PaxEstimated": null,
      |        "Runway": "3",
      |        "ScheduledDateTime": "2019-11-13T12:34:00+00:00",
      |        "StandCode": "ST",
      |        "TerminalCode": "T1",
      |        "TotalPassengerCount": 20
      |}]""".stripMargin

  "Given a different arrival with valid GLA Feed Json I should get back a valid Arrival object" >> {
    val mockFeed = mockFeedWithResponse(secondJsonExample)

    val expected = Arrival(
      operator = None,
      status = ArrivalStatus("Flight is cancelled"),
      estimated = None,
      actual = Some(SDate("2019-11-14T14:41:00Z").millisSinceEpoch),
      estimatedChox = Some(SDate("2019-11-14T12:44:00Z").millisSinceEpoch),
      actualChox = Some(SDate("2019-11-14T14:40:00Z").millisSinceEpoch),
      gate = Some("GATE"),
      stand = Some("STAND"),
      maxPax = Some(75),
      actPax = Some(55),
      tranPax = None,
      runwayID = Some("4"),
      baggageReclaimId = Some("2"),
      airportID = PortCode("GLA"),
      terminal = T1,
      rawICAO = "TTT244",
      rawIATA = "TT244",
      origin = PortCode("TTT"),
      scheduled = SDate("2019-11-14T12:44:00Z").millisSinceEpoch,
      pcpTime = None,
      feedSources = Set(LiveFeedSource),
      carrierScheduled = None
    )

    Await.result(mockFeed.requestArrivals(), 1 second) match {
      case ArrivalsFeedSuccess(Flights(arrival :: Nil), _) => arrival === expected
    }
  }


  "Given a GLA feed item with 0 for ActPax and MaxPax then we should 0 in the arrival" >> {
    val mockFeed = mockFeedWithResponse(exampleWith0s)

    Await.result(mockFeed.requestArrivals(), 1 second) match {
      case ArrivalsFeedSuccess(Flights(arrival :: Nil), _) =>
        (arrival.actPax, arrival.maxPax) === (Some(0), Some(0))
    }
  }

  val secondJsonExample: String =
    """[{
      |        "AIBT": "2019-11-14T14:40:00+00:00",
      |        "AirlineIATA": "TT",
      |        "AirlineICAO": "TTT",
      |        "ALDT": "2019-11-14T14:41:00+00:00",
      |        "AODBProbableDateTime": null,
      |        "CarouselCode": "2",
      |        "CodeShareFlights": "",
      |        "CodeShareInd": "N",
      |        "DepartureArrivalType": "A",
      |        "EIBT": "2019-11-14T12:44:00+00:00",
      |        "FlightNumber": "244",
      |        "FlightStatus": "C",
      |        "FlightStatusDesc": "Flight is cancelled",
      |        "GateCode": "GATE",
      |        "MaxPax": 75,
      |        "OriginDestAirportIATA": "TTT",
      |        "OriginDestAirportICAO": "TTTT",
      |        "PaxEstimated": null,
      |        "Runway": "4",
      |        "ScheduledDateTime": "2019-11-14T12:44:00+00:00",
      |        "StandCode": "STAND",
      |        "TerminalCode": "T1",
      |        "TotalPassengerCount": 55
      |}]""".stripMargin

  val exampleWith0s: String =
    """[{
      |        "AIBT": "2019-11-14T14:40:00+00:00",
      |        "AirlineIATA": "TT",
      |        "AirlineICAO": "TTT",
      |        "ALDT": "2019-11-14T14:41:00+00:00",
      |        "AODBProbableDateTime": null,
      |        "CarouselCode": "2",
      |        "CodeShareFlights": "",
      |        "CodeShareInd": "N",
      |        "DepartureArrivalType": "A",
      |        "EIBT": "2019-11-14T12:44:00+00:00",
      |        "FlightNumber": "244",
      |        "FlightStatus": "C",
      |        "FlightStatusDesc": "Flight is cancelled",
      |        "GateCode": "GATE",
      |        "MaxPax": 0,
      |        "OriginDestAirportIATA": "TTT",
      |        "OriginDestAirportICAO": "TTTT",
      |        "PaxEstimated": null,
      |        "Runway": "4",
      |        "ScheduledDateTime": "2019-11-14T12:44:00+00:00",
      |        "StandCode": "STAND",
      |        "TerminalCode": "T1",
      |        "TotalPassengerCount": 0
      |}]""".stripMargin


  "Given a different arrival with only required JSON fields then I should still get an arrival object with those fields" >> {
    val mockFeed = mockFeedWithResponse(requiredFieldsOnlyJson)

    val expected = Arrival(
      operator = None,
      status = ArrivalStatus("Flight is cancelled"),
      estimated = None,
      actual = None,
      estimatedChox = None,
      actualChox = None,
      gate = None,
      stand = None,
      maxPax = None,
      actPax = None,
      tranPax = None,
      runwayID = None,
      baggageReclaimId = None,
      airportID = PortCode("GLA"),
      terminal = T1,
      rawICAO = "TTT244",
      rawIATA = "TT244",
      origin = PortCode("TTT"),
      scheduled = SDate("2019-11-14T12:44:00Z").millisSinceEpoch,
      pcpTime = None,
      feedSources = Set(LiveFeedSource),
      carrierScheduled = None
    )

    Await.result(mockFeed.requestArrivals(), 1 second) match {
      case ArrivalsFeedSuccess(Flights(arrival :: Nil), _) => arrival === expected
    }
  }

  val requiredFieldsOnlyJson: String =
    """[{
      |        "AIBT": null,
      |        "AirlineIATA": "TT",
      |        "AirlineICAO": "TTT",
      |        "ALDT": null,
      |        "AODBProbableDateTime": null,
      |        "CarouselCode": null,
      |        "CodeShareFlights": null,
      |        "CodeShareInd": null,
      |        "DepartureArrivalType": "A",
      |        "EIBT": null,
      |        "FlightNumber": "244",
      |        "FlightStatus": "C",
      |        "FlightStatusDesc": "Flight is cancelled",
      |        "GateCode": null,
      |        "MaxPax": null,
      |        "OriginDestAirportIATA": "TTT",
      |        "OriginDestAirportICAO": "TTTT",
      |        "PaxEstimated": null,
      |        "Runway": null,
      |        "ScheduledDateTime": "2019-11-14T12:44:00+00:00",
      |        "StandCode": null,
      |        "TerminalCode": "T1",
      |        "TotalPassengerCount": null
      |}]""".stripMargin

  def containingADepartureJson(domesticScheduledTime: String): String =
    s"""[{
        |      "AIBT": "2019-11-13T13:30:00+00:00",
        |      "AirlineIATA": "TS",
        |      "AirlineICAO": "TST",
        |      "ALDT": "2019-11-13T13:31:00+00:00",
        |      "AODBProbableDateTime": "2019-11-13T13:32:00+00:00",
        |      "CarouselCode": "2",
        |      "CodeShareFlights": "",
        |      "CodeShareInd": "N",
        |      "DepartureArrivalType": "A",
        |      "EIBT": "2019-11-13T12:33:00+00:00",
        |      "FlightNumber": "234",
        |      "FlightStatus": "S",
        |      "FlightStatusDesc": "Flight is on schedule",
        |      "GateCode": "G",
        |      "MaxPax": 50,
        |      "OriginDestAirportIATA": "TST",
        |      "OriginDestAirportICAO": "TSTT",
        |      "PaxEstimated": null,
        |      "Runway": "3",
        |      "ScheduledDateTime": "2019-11-13T12:34:00+00:00",
        |      "StandCode": "ST",
        |      "TerminalCode": "T1",
        |      "TotalPassengerCount": 20
        |},
        |{
        |      "AIBT": null,
        |      "AirlineIATA": "TT",
        |      "AirlineICAO": "TTT",
        |      "ALDT": null,
        |      "AODBProbableDateTime": null,
        |      "CarouselCode": null,
        |      "CodeShareFlights": null,
        |      "CodeShareInd": null,
        |      "DepartureArrivalType": "D",
        |      "EIBT": null,
        |      "FlightNumber": "244",
        |      "FlightStatus": "C",
        |      "FlightStatusDesc": "Flight is cancelled",
        |      "GateCode": null,
        |      "MaxPax": null,
        |      "OriginDestAirportIATA": "TTT",
        |      "OriginDestAirportICAO": "TTTT",
        |      "PaxEstimated": null,
        |      "Runway": null,
        |      "ScheduledDateTime": "$domesticScheduledTime",
        |      "StandCode": null,
        |      "TerminalCode": "T1",
        |      "TotalPassengerCount": null
        |}]""".stripMargin

}
