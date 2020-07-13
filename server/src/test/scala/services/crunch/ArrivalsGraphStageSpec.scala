package services.crunch

import actors.FlightMessageConversion
import controllers.ArrivalGenerator
import controllers.ArrivalGenerator.arrival
import drt.shared.CrunchApi.{CrunchMinute, StaffMinute}
import drt.shared.FlightsApi.Flights
import drt.shared.PaxTypes.EeaMachineReadable
import drt.shared.Queues.EeaDesk
import drt.shared.SplitRatiosNs.SplitSources.TerminalAverage
import drt.shared.Terminals.T1
import drt.shared._
import drt.shared.api.Arrival
import org.specs2.matcher.Scope
import passengersplits.core.PassengerTypeCalculatorValues.DocumentType
import passengersplits.parsing.VoyageManifestParser._
import server.feeds.{ArrivalsFeedResponse, ArrivalsFeedSuccess, DqManifests, ManifestsFeedResponse, ManifestsFeedSuccess}
import services.SDate

import scala.collection.immutable.{List, SortedMap}
import scala.collection.mutable
import scala.concurrent.duration._

class ArrivalsGraphStageSpec extends CrunchTestLike {
  sequential
  isolated

  val dateNow: SDateLike = SDate("2017-01-01T00:00Z")

  val arrival_v1_with_no_chox_time: Arrival = arrival(iata = "BA0001", schDt = "2017-01-01T10:25Z", actPax = Option(100), origin = PortCode("JFK"), feedSources = Set(LiveFeedSource))

  val arrival_v2_with_chox_time: Arrival = arrival_v1_with_no_chox_time.copy(stand = Option("Stand1"), actualChox = Option(SDate("2017-01-01T10:25Z").millisSinceEpoch))

  val terminalSplits: Splits = Splits(Set(ApiPaxTypeAndQueueCount(EeaMachineReadable, EeaDesk, 100.0, None)), TerminalAverage, None, Percentage)

  val initialPortState: Option[PortState] = Option(PortState(SortedMap(arrival_v2_with_chox_time.unique -> ApiFlightWithSplits(arrival_v2_with_chox_time, Set(terminalSplits))), SortedMap[TQM, CrunchMinute](), SortedMap[TM, StaffMinute]()))
  val initialLiveArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap[UniqueArrival, Arrival]() ++ List(arrival_v2_with_chox_time).map(a => (a.unique, a))

  "Given and Arrivals Graph Stage" should {
    val airportConfig = defaultAirportConfig.copy(queuesByTerminal = defaultAirportConfig.queuesByTerminal.filterKeys(_ == T1))

    "a third arrival with an update to the chox time will change the arrival" >> {
      val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(airportConfig = airportConfig, now = () => dateNow, initialPortState = initialPortState, initialLiveArrivals = initialLiveArrivals))
      val arrival_v3_with_an_update_to_chox_time: Arrival = arrival_v2_with_chox_time.copy(actualChox = Option(SDate("2017-01-01T10:30Z").millisSinceEpoch), stand = Option("I will update"))
      offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(arrival_v3_with_an_update_to_chox_time))))

      val expectedArrivals: List[Arrival] = List(arrival_v3_with_an_update_to_chox_time)

      crunch.portStateTestProbe.fishForMessage(5 seconds) {
        case ps: PortState =>
          val arrivals = ps.flights.values.map(_.apiFlight)
          arrivals == expectedArrivals
      }

      success
    }

    "once an API (advanced passenger information) input arrives for the flight, it will update the arrivals FeedSource so that it has a LiveFeed and a ApiFeed" >> {
      val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(airportConfig = airportConfig, now = () => dateNow, initialPortState = initialPortState, initialLiveArrivals = initialLiveArrivals))
      val voyageManifests: ManifestsFeedResponse = ManifestsFeedSuccess(DqManifests("", Set(
        VoyageManifest(EventTypes.DC, PortCode("STN"), PortCode("JFK"), VoyageNumber("0001"), CarrierCode("BA"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("10:25"), List(
          PassengerInfoJson(Option(DocumentType("P")), Nationality("GBR"), EeaFlag("EEA"), Option(PaxAge(22)), Option(PortCode("LHR")), InTransit("N"), Option(Nationality("GBR")), Option(Nationality("GBR")), None)
          ))
        )))

      offerAndWait(crunch.manifestsLiveInput, voyageManifests)

      val expected = Set(LiveFeedSource, ApiFeedSource)

      crunch.portStateTestProbe.fishForMessage(5 seconds) {
        case ps: PortState =>
          val portStateSources = ps.flights.values.flatMap(_.apiFlight.feedSources).toSet
          portStateSources == expected
      }

      success
    }

    "once an acl and a forecast input arrives for the flight, it will update the arrivals FeedSource so that it has ACLFeed and ForecastFeed" >> {
      val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(airportConfig = airportConfig, now = () => dateNow, initialPortState = initialPortState, initialLiveArrivals = initialLiveArrivals))
      val forecastScheduled = "2017-01-01T10:25Z"

      val aclFlight: Flights = Flights(List(
        ArrivalGenerator.arrival(iata = "BA0002", schDt = forecastScheduled, actPax = Option(10), feedSources = Set(AclFeedSource))
        ))

      offerAndWait(crunch.baseArrivalsInput, ArrivalsFeedSuccess(aclFlight))

      val forecastArrival: Arrival = arrival(schDt = forecastScheduled, iata = "BA0002", terminal = T1, actPax = Option(21), feedSources = Set(ForecastFeedSource))
      val forecastArrivals: ArrivalsFeedResponse = ArrivalsFeedSuccess(Flights(List(forecastArrival)))

      offerAndWait(crunch.forecastArrivalsInput, forecastArrivals)

      val expected = Set(ForecastFeedSource, AclFeedSource)

      crunch.portStateTestProbe.fishForMessage(5 seconds) {
        case ps: PortState =>
          val portStateSources = ps.flights.get(forecastArrival.unique).map(_.apiFlight.feedSources).getOrElse(Set())

          portStateSources == expected
      }

      success
    }

    "Given 2 arrivals, one international and the other domestic " +
      "I should only see the international arrival in the port state" >> {
      val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(airportConfig = airportConfig, now = () => dateNow))
      val scheduled = "2017-01-01T10:25Z"

      val arrivalInt: Arrival = ArrivalGenerator.arrival(iata = "BA0002", origin = PortCode("JFK"), schDt = scheduled, actPax = Option(10), feedSources = Set(AclFeedSource))
      val arrivalDom: Arrival = ArrivalGenerator.arrival(iata = "BA0003", origin = PortCode("BHX"), schDt = scheduled, actPax = Option(10), feedSources = Set(AclFeedSource))

      val aclFlight: Flights = Flights(List(arrivalInt, arrivalDom))

      offerAndWait(crunch.baseArrivalsInput, ArrivalsFeedSuccess(aclFlight))

      crunch.portStateTestProbe.fishForMessage(5 seconds) {
        case ps: PortState => flightExists(arrivalInt, ps) && !flightExists(arrivalDom, ps)
      }

      success
    }
  }

  "Given an empty PortState I should only see arrivals without a suffix in the port state" >> {
    val withSuffixP: Arrival = ArrivalGenerator.arrival(iata = "BA0001P", origin = PortCode("JFK"), schDt = "2017-01-01T10:25Z", actPax = Option(10), feedSources = Set(AclFeedSource))
    val withSuffixF: Arrival = ArrivalGenerator.arrival(iata = "BA0002F", origin = PortCode("JFK"), schDt = "2017-01-01T11:25Z", actPax = Option(10), feedSources = Set(AclFeedSource))
    val withoutSuffix: Arrival = ArrivalGenerator.arrival(iata = "BA0003", origin = PortCode("JFK"), schDt = "2017-01-01T12:25Z", actPax = Option(10), feedSources = Set(AclFeedSource))

    "Given 3 international ACL arrivals, one with suffix F, another with P, and another with no suffix" >> {
      val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(now = () => dateNow))
      val aclFlight: Flights = Flights(List(withSuffixP, withSuffixF, withoutSuffix))

      offerAndWait(crunch.baseArrivalsInput, ArrivalsFeedSuccess(aclFlight))

      crunch.portStateTestProbe.fishForMessage(5 seconds) {
        case ps: PortState =>
          val numberOfFlights = ps.flights.size
          numberOfFlights == 1 && flightExists(withoutSuffix, ps)
      }

      success
    }

    "Given 3 international live arrivals, one with suffix F, another with P, and another with no suffix" >> {
      val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(now = () => dateNow))
      val aclFlight: Flights = Flights(List(withSuffixP, withSuffixF, withoutSuffix))

      offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(aclFlight))

      crunch.portStateTestProbe.fishForMessage(5 seconds) {
        case ps: PortState =>
          val numberOfFlights = ps.flights.size
          numberOfFlights == 1 && flightExists(withoutSuffix, ps)
      }

      success
    }
  }

  private def flightExists(withoutSuffix: Arrival, ps: PortState) = {
    ps.flights.contains(UniqueArrival(withoutSuffix))
  }
}
