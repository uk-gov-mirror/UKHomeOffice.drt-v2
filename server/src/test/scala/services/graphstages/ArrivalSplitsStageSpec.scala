package services.graphstages

import actors.acking.AckingReceiver.StreamCompleted
import akka.stream.scaladsl.{GraphDSL, RunnableGraph, Sink, Source, SourceQueueWithComplete}
import akka.stream.{ClosedShape, OverflowStrategy}
import akka.testkit.TestProbe
import controllers.ArrivalGenerator
import drt.shared.FlightsApi.FlightsWithSplitsDiff
import drt.shared.PaxTypes.EeaMachineReadable
import drt.shared.SplitRatiosNs.{SplitRatio, SplitRatios, SplitSources}
import drt.shared.Terminals.T1
import drt.shared._
import drt.shared.api.Arrival
import manifests.passengers.BestAvailableManifest
import manifests.queues.SplitsCalculator
import passengersplits.core.PassengerTypeCalculatorValues.DocumentType
import passengersplits.parsing.VoyageManifestParser.{ManifestDateOfArrival, ManifestTimeOfArrival, VoyageManifest}
import queueus.{B5JPlusWithTransitTypeAllocator, PaxTypeQueueAllocation, TerminalQueueAllocatorWithFastTrack}
import services.SDate
import services.crunch.{CrunchTestLike, PassengerInfoGenerator}

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._


object TestableArrivalSplits {
  val oneDayMillis: Int = 60 * 60 * 24 * 1000

  def groupByCodeShares(flights: Seq[ApiFlightWithSplits]): Seq[(ApiFlightWithSplits, Set[Arrival])] = flights.map(f => (f, Set(f.apiFlight)))

  def apply(splitsCalculator: SplitsCalculator, testProbe: TestProbe, now: () => SDateLike): RunnableGraph[(SourceQueueWithComplete[ArrivalsDiff], SourceQueueWithComplete[List[BestAvailableManifest]], SourceQueueWithComplete[List[BestAvailableManifest]])] = {
    val arrivalSplitsStage = new ArrivalSplitsGraphStage(
      name = "",
      optionalInitialFlights = None,
      splitsCalculator = splitsCalculator,
      expireAfterMillis = oneDayMillis,
      now = now
    )

    val arrivalsDiffSource = Source.queue[ArrivalsDiff](1, OverflowStrategy.backpressure)
    val manifestsLiveSource = Source.queue[List[BestAvailableManifest]](1, OverflowStrategy.backpressure)
    val manifestsHistoricSource = Source.queue[List[BestAvailableManifest]](1, OverflowStrategy.backpressure)

    import akka.stream.scaladsl.GraphDSL.Implicits._

    val graph = GraphDSL.create(
      arrivalsDiffSource.async,
      manifestsLiveSource.async,
      manifestsHistoricSource.async
    )((_, _, _)) {

      implicit builder =>
        (
          arrivalsDiff,
          manifestsLive,
          manifestsHistoric
        ) =>
          val arrivalSplitsStageAsync = builder.add(arrivalSplitsStage.async)
          val sink = builder.add(Sink.actorRef(testProbe.ref, StreamCompleted))

          arrivalsDiff.out ~> arrivalSplitsStageAsync.in0
          manifestsLive.out ~> arrivalSplitsStageAsync.in1
          manifestsHistoric.out ~> arrivalSplitsStageAsync.in2

          arrivalSplitsStageAsync.out ~> sink

          ClosedShape
    }

    RunnableGraph.fromGraph(graph)
  }
}

class ArrivalSplitsStageSpec extends CrunchTestLike {
  val portCode = PortCode("LHR")
  val splitsProvider: (String, MilliDate) => Option[SplitRatios] = (_, _) => {
    val eeaMrToDeskSplit = SplitRatio(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk), 0.5)
    val eeaNmrToDeskSplit = SplitRatio(PaxTypeAndQueue(PaxTypes.EeaNonMachineReadable, Queues.EeaDesk), 0.5)
    Option(SplitRatios(List(eeaMrToDeskSplit, eeaNmrToDeskSplit), SplitSources.Historical))
  }

  val paxTypeQueueAllocation = PaxTypeQueueAllocation(
    B5JPlusWithTransitTypeAllocator(),
    TerminalQueueAllocatorWithFastTrack(defaultAirportConfig.terminalPaxTypeQueueAllocation))

  val splitsCalculator = SplitsCalculator(paxTypeQueueAllocation, defaultAirportConfig.terminalPaxSplits)

  "Given an arrival splits stage " +
    "When I push an arrival and some splits for that arrival " +
    "Then I should see a message containing a FlightWithSplits representing them" >> {

    val arrivalDate = "2018-01-01"
    val arrivalTime = "00:05"
    val scheduled = s"${arrivalDate}T$arrivalTime"
    val probe = TestProbe("arrival-splits")

    val (arrivalDiffs, manifestsLiveInput, _) = TestableArrivalSplits(splitsCalculator, probe, () => SDate(scheduled)).run()
    val arrival = ArrivalGenerator.arrival(iata = "BA0001", terminal = T1, origin = PortCode("JFK"), schDt = scheduled, feedSources = Set(LiveFeedSource))
    val paxList = List(
      PassengerInfoGenerator.passengerInfoJson(nationality = Nationality("GBR"), documentType = DocumentType("P"), issuingCountry = Nationality("GBR")),
      PassengerInfoGenerator.passengerInfoJson(nationality = Nationality("ITA"), documentType = DocumentType("P"), issuingCountry = Nationality("ITA"))
    )
    val manifests = Set(VoyageManifest(EventTypes.DC, portCode, PortCode("JFK"), VoyageNumber("0001"), CarrierCode("BA"), ManifestDateOfArrival(arrivalDate), ManifestTimeOfArrival(arrivalTime), PassengerList = paxList))

    arrivalDiffs.offer(ArrivalsDiff(toUpdate = SortedMap(arrival.unique -> arrival), toRemove = Set()))

    probe.fishForMessage(3 seconds) {
      case FlightsWithSplitsDiff(flights, _) => flights.nonEmpty
    }

    manifestsLiveInput.offer(manifests.map(BestAvailableManifest(_)).toList)

    val terminalAverage = Splits(Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 100.0, None)), SplitSources.TerminalAverage, None, Percentage)
    val apiSplits = Splits(
      Set(
        ApiPaxTypeAndQueueCount(EeaMachineReadable, Queues.EGate, 1.6, Some(Map(Nationality("GBR") -> 0.8, Nationality("ITA") -> 0.8))),
        ApiPaxTypeAndQueueCount(EeaMachineReadable, Queues.EeaDesk, 0.4, Some(Map(Nationality("GBR") -> 0.2, Nationality("ITA") -> 0.2)))),
      SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages, None, PaxNumbers)

    val expectedSplits = Set(terminalAverage, apiSplits)
    val expected = Seq(ApiFlightWithSplits(
      arrival.copy(feedSources = Set(LiveFeedSource, ApiFeedSource), apiPax = Option(2)),
      expectedSplits,
      None
    ))

    probe.fishForMessage(3 seconds) {
      case fs: FlightsWithSplitsDiff =>
        val fws = fs.flightsToUpdate.map(f => f.copy(lastUpdated = None))
        fws === expected
    }

    true
  }
}
