package services.crunch.workload

import controllers.ArrivalGenerator
import drt.shared.SplitRatiosNs.SplitSources
import drt.shared._
import drt.shared.api.Arrival
import org.specs2.mutable.Specification
import services.SDate
import services.graphstages.WorkloadCalculator

class WorkloadSpec extends Specification {
  val gbrSeconds = 45d
  val fraSeconds = 45d
  val zaSeconds = 100d

  val natProcTimes: Map[Nationality, Double] = Map(
    Nationality("GBR") -> gbrSeconds,
    Nationality("FRA") -> fraSeconds,
    Nationality("ZAR") -> zaSeconds
  )

  def pcpPaxFn: Arrival => Int = PcpPax.bestPaxEstimateWithApi

  "Given an arrival with 1 pax and 1 split containing 1 pax with no nationality data " +
    "When I ask for the workload for this arrival " +
    "Then I see the 1x the proc time provided" >> {

    val arrival = ArrivalGenerator.arrival(actPax = Option(1))
    val splits = Set(
      Splits(
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, None)),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(EventTypes.DC),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val emptyNatProcTimes: Map[Nationality, Double] = Map()
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(
        ApiFlightWithSplits(arrival, splits, None),
        procTimes,
        emptyNatProcTimes,
        true,
        pcpPaxFn
      )
      .toList
      .map(_.workLoad)

    workloads === List(1.5)
  }

  "Given an arrival with a PCP time that has seconds, then these seconds should be ignored for workload calcs" >> {
    val arrival = ArrivalGenerator.arrival(actPax = Option(1))
      .copy(pcpTime = Some(SDate("2018-08-28T17:07:05").millisSinceEpoch))

    val splits = Set(
      Splits(
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, None)),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(EventTypes.DC),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)

    val flightSplitMinutes = WorkloadCalculator
      .flightToFlightSplitMinutes(
        ApiFlightWithSplits(arrival, splits, None),
        procTimes,
        Map()
        , false, pcpPaxFn)
      .toList

    val startTime = SDate(flightSplitMinutes.head.minute).toISOString()

    startTime === "2018-08-28T17:07:00Z"
  }

  "Given an arrival with 1 pax and 1 split containing 1 pax with 1 nationality " +
    "When I ask for the workload for this arrival " +
    "Then I see the 1x the proc time for the given nationality rather than the port proc time" >> {

    val arrival = ArrivalGenerator.arrival(actPax = Option(1))
    val splits = Set(
      Splits(
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, Option(Map(Nationality("GBR") -> 1)))),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(EventTypes.DC),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(
        ApiFlightWithSplits(arrival, splits, None),
        procTimes,
        natProcTimes,
        true,
        pcpPaxFn
      )
      .toList
      .map(_.workLoad)

    workloads === List(gbrSeconds / 60)
  }

  "Given an arrival with 10 pax and 1 split containing 1 pax with 1 nationality " +
    "When I ask for the workload for this arrival " +
    "Then I see the 10x the proc time for the given nationality" >> {

    val arrival = ArrivalGenerator.arrival(actPax = Option(10))
    val splits = Set(
      Splits(
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, Option(Map(Nationality("GBR") -> 1)))),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(EventTypes.DC),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(
        ApiFlightWithSplits(arrival, splits, None),
        procTimes,
        natProcTimes,
        true,
        pcpPaxFn
      )
      .toList
      .map(_.workLoad)

    workloads === List(gbrSeconds / 60 * 10)
  }

  "Given an arrival with 100 pax and 1 split containing 1 pax with 1 nationality " +
    "When I ask for the workload for this arrival " +
    "Then I see the 100x the proc time for the given nationality" >> {

    val arrival = ArrivalGenerator.arrival(actPax = Option(100))
    val splits = Set(
      Splits(
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, Option(Map(Nationality("GBR") -> 1)))),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(EventTypes.DC),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(
        ApiFlightWithSplits(arrival, splits, None),
        procTimes,
        natProcTimes,
        true,
        pcpPaxFn
      )
      .toList
      .map(_.workLoad)

    workloads === List.fill(5)(gbrSeconds / 60 * 20)
  }

  "Given an arrival with 10 pax and 1 split containing 3 pax with 2 nationalities " +
    "When I ask for the workload for this arrival " +
    "Then I see the 10x 2/3 + 10x 1/3 the proc times for the given nationalities" >> {

    val arrival = ArrivalGenerator.arrival(actPax = Option(10))
    val splits = Set(
      Splits(
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 3, Option(Map(Nationality("GBR") -> 1, Nationality("FRA") -> 2)))),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(EventTypes.DC),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(
        ApiFlightWithSplits(arrival, splits, None),
        procTimes,
        natProcTimes,
        true,
        pcpPaxFn
      )
      .toList
      .map(_.workLoad)

    val expectedFraWorkload = 2d / 3 * (fraSeconds / 60) * 10
    val expectedGbrWorkload = 1d / 3 * (gbrSeconds / 60) * 10
    workloads === List(expectedFraWorkload + expectedGbrWorkload)
  }

  "Given an arrival with 2 pax and 2 splits each containing 1 pax with 1 nationality " +
    "When I ask for the workload for this arrival " +
    "Then I see the 1x the proc time for the given nationality in each queue" >> {

    val arrival = ArrivalGenerator.arrival(actPax = Option(2))
    val splits = Set(
      Splits(
        Set(
          ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, Option(Map(Nationality("GBR") -> 1))),
          ApiPaxTypeAndQueueCount(PaxTypes.VisaNational, Queues.NonEeaDesk, 1, Option(Map(Nationality("ZAR") -> 1)))
        ),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(EventTypes.DC),
        PaxNumbers))
    val procTimes = Map(
      PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5,
      PaxTypeAndQueue(PaxTypes.VisaNational, Queues.NonEeaDesk) -> 5.5
    )
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(
        ApiFlightWithSplits(arrival, splits, None),
        procTimes,
        natProcTimes,
        true,
        pcpPaxFn
      )
      .toList
      .map(_.workLoad)

    workloads === List(gbrSeconds / 60, zaSeconds / 60)
  }

  "Given an arrival with 4 pax and 2 splits each containing 2 pax with 2 nationalities " +
    "When I ask for the workload for this arrival " +
    "Then I see the sum to each nationality for each split" >> {

    val arrival = ArrivalGenerator.arrival(actPax = Option(4))
    val splits = Set(
      Splits(
        Set(
          ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 2, Option(Map(Nationality("GBR") -> 1, Nationality("FRA") -> 1))),
          ApiPaxTypeAndQueueCount(PaxTypes.VisaNational, Queues.NonEeaDesk, 2, Option(Map(Nationality("ZAR") -> 1, Nationality("ZBW") -> 1)))
        ),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(EventTypes.DC),
        PaxNumbers))
    val procTimes = Map(
      PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5,
      PaxTypeAndQueue(PaxTypes.VisaNational, Queues.NonEeaDesk) -> 5.5
    )
    val gbrSeconds = 45d
    val fraSeconds = 90d
    val zaSeconds = 100d
    val zbwSeconds = 200d
    val natProcTimes: Map[Nationality, Double] = Map(
      Nationality("GBR") -> gbrSeconds,
      Nationality("FRA") -> fraSeconds,
      Nationality("ZBW") -> zbwSeconds,
      Nationality("ZAR") -> zaSeconds
    )
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(
        ApiFlightWithSplits(arrival, splits, None),
        procTimes,
        natProcTimes,
        true,
        pcpPaxFn
      )
      .toList
      .map(_.workLoad)

    workloads === List((gbrSeconds + fraSeconds) / 60, (zaSeconds + zbwSeconds) / 60)
  }

  "Given an arrival with 30 pax and 2 splits each containing 15 pax with 2 nationalities " +
    "When I ask for the workload for this arrival " +
    "Then I see the sum to each nationality for each split over two minutes" >> {

    val arrival = ArrivalGenerator.arrival(actPax = Option(24))
    val splits = Set(
      Splits(
        Set(
          ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 12, Option(Map(Nationality("GBR") -> 10, Nationality("FRA") -> 2))),
          ApiPaxTypeAndQueueCount(PaxTypes.VisaNational, Queues.NonEeaDesk, 12, Option(Map(Nationality("ZAR") -> 6, Nationality("ZBW") -> 6)))
        ),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(EventTypes.DC),
        PaxNumbers))
    val procTimes = Map(
      PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5,
      PaxTypeAndQueue(PaxTypes.VisaNational, Queues.NonEeaDesk) -> 5.5
    )
    val gbrSeconds = 45d
    val fraSeconds = 90d
    val zaSeconds = 100d
    val zbwSeconds = 200d
    val natProcTimes: Map[Nationality, Double] = Map(
      Nationality("GBR") -> gbrSeconds,
      Nationality("FRA") -> fraSeconds,
      Nationality("ZBW") -> zbwSeconds,
      Nationality("ZAR") -> zaSeconds
    )
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(
        ApiFlightWithSplits(arrival, splits, None),
        procTimes,
        natProcTimes,
        true,
        pcpPaxFn
      )
      .map(m => (m.paxType, m.queueName, m.workLoad))

    val eeaDeskWorkloadInSeconds = (gbrSeconds * 10 + fraSeconds * 2) / 60
    val nonEEADeskWorkloadInSeconds = (zaSeconds * 6 + zbwSeconds * 6) / 60
    workloads.toSet === Set(
      (PaxTypes.EeaMachineReadable, Queues.EeaDesk, eeaDeskWorkloadInSeconds * 10 / 12),
      (PaxTypes.VisaNational, Queues.NonEeaDesk, nonEEADeskWorkloadInSeconds * 10 / 12),
      (PaxTypes.EeaMachineReadable, Queues.EeaDesk, eeaDeskWorkloadInSeconds * 2 / 12),
      (PaxTypes.VisaNational, Queues.NonEeaDesk, nonEEADeskWorkloadInSeconds * 2 / 12)
    )
  }

  "Given an arrival with None pax on the arrival and 1 split containing 6 pax with no nationality data " +
    "When I ask for the workload for this arrival " +
    "Then I see the 6x the proc time provided" >> {

    val arrival = ArrivalGenerator.arrival(actPax = None, apiPax = Option(6))
    val splits = Set(
      Splits(
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 6, None)),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(EventTypes.DC),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val emptyNatProcTimes: Map[Nationality, Double] = Map()
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(
        ApiFlightWithSplits(arrival, splits, None),
        procTimes,
        emptyNatProcTimes,
        true,
        PcpPax.bestPaxEstimateWithApi
      )
      .toList
      .map(_.workLoad)

    workloads === List(1.5 * 6)
  }

  "Given an arrival with 1 pax on the arrival and 1 split containing 6 pax with no nationality data " +
    "When I ask for the workload for this arrival " +
    "Then I see the 1x the proc time provided" >> {

    val arrival = ArrivalGenerator.arrival(actPax = Option(1), apiPax = Option(6), feedSources = Set(LiveFeedSource))
    val splits = Set(
      Splits(
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 6, None)),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(EventTypes.DC),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val emptyNatProcTimes: Map[Nationality, Double] = Map()
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(
        ApiFlightWithSplits(arrival, splits, None),
        procTimes,
        emptyNatProcTimes,
        true,
        PcpPax.bestPaxEstimateWithApi
      )
      .toList
      .map(_.workLoad)

    workloads === List(1.5 * 1)
  }
}
