package services.crunch.workload

import controllers.ArrivalGenerator
import drt.shared.FlightsApi.{QueueName, TerminalName}
import drt.shared.SplitRatiosNs.SplitSources
import drt.shared._
import org.specs2.mutable.Specification
import services.SDate
import services.graphstages.Crunch.LoadMinute
import services.graphstages.{Crunch, WorkloadCalculator}

import scala.collection.immutable.SortedMap

class WorkloadSpec extends Specification {
  "Given an arrival with 1 pax and 1 split containing 1 pax with no nationality data " +
    "When I ask for the workload for this arrival " +
    "Then I see the 1x the proc time provided" >> {

    val arrival = ArrivalGenerator.arrival(actPax = Option(1))
    val splits = Set(
      Splits(
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, None)),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(DqEventCodes.DepartureConfirmed),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val emptyNatProcTimes: Map[String, Double] = Map()
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(ApiFlightWithSplits(arrival, splits, None), procTimes, emptyNatProcTimes, true)
      .toList
      .map(_.workLoad)

    workloads === List(1.5)
  }

  "Given an arrival with a PCP time that has seconds, then these seconds should be ignored for workload calcs" >> {
    val arrival = ArrivalGenerator.arrival(actPax = Option(1))
      .copy(PcpTime = Some(SDate("2018-08-28T17:07:05").millisSinceEpoch))

    val splits = Set(
      Splits(
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, None)),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(DqEventCodes.DepartureConfirmed),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)

    val flightSplitMinutes = WorkloadCalculator
      .flightToFlightSplitMinutes(ApiFlightWithSplits(arrival, splits, None), procTimes, Map(), false)
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
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, Option(Map("GBR" -> 1)))),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(DqEventCodes.DepartureConfirmed),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val gbrSeconds = 45d
    val natProcTimes: Map[String, Double] = Map("GBR" -> gbrSeconds)
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(ApiFlightWithSplits(arrival, splits, None), procTimes, natProcTimes, true)
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
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, Option(Map("GBR" -> 1)))),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(DqEventCodes.DepartureConfirmed),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val gbrSeconds = 45d
    val natProcTimes: Map[String, Double] = Map("GBR" -> gbrSeconds)
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(ApiFlightWithSplits(arrival, splits, None), procTimes, natProcTimes, true)
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
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, Option(Map("GBR" -> 1)))),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(DqEventCodes.DepartureConfirmed),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val gbrSeconds = 45d
    val natProcTimes: Map[String, Double] = Map("GBR" -> gbrSeconds)
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(ApiFlightWithSplits(arrival, splits, None), procTimes, natProcTimes, true)
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
        Set(ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 3, Option(Map("GBR" -> 1, "FRA" -> 2)))),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(DqEventCodes.DepartureConfirmed),
        PaxNumbers))
    val procTimes = Map(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5)
    val gbrSeconds = 45d
    val fraSeconds = 45d
    val natProcTimes: Map[String, Double] = Map("GBR" -> gbrSeconds, "FRA" -> fraSeconds)
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(ApiFlightWithSplits(arrival, splits, None), procTimes, natProcTimes, true)
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
          ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 1, Option(Map("GBR" -> 1))),
          ApiPaxTypeAndQueueCount(PaxTypes.VisaNational, Queues.NonEeaDesk, 1, Option(Map("ZAR" -> 1)))
        ),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(DqEventCodes.DepartureConfirmed),
        PaxNumbers))
    val procTimes = Map(
      PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5,
      PaxTypeAndQueue(PaxTypes.VisaNational, Queues.NonEeaDesk) -> 5.5
    )
    val gbrSeconds = 45d
    val zaSeconds = 100d
    val natProcTimes: Map[String, Double] = Map("GBR" -> gbrSeconds, "ZAR" -> zaSeconds)
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(ApiFlightWithSplits(arrival, splits, None), procTimes, natProcTimes, true)
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
          ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 2, Option(Map("GBR" -> 1, "FRA" -> 1))),
          ApiPaxTypeAndQueueCount(PaxTypes.VisaNational, Queues.NonEeaDesk, 2, Option(Map("ZAR" -> 1, "ZBW" -> 1)))
        ),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(DqEventCodes.DepartureConfirmed),
        PaxNumbers))
    val procTimes = Map(
      PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5,
      PaxTypeAndQueue(PaxTypes.VisaNational, Queues.NonEeaDesk) -> 5.5
    )
    val gbrSeconds = 45d
    val fraSeconds = 90d
    val zaSeconds = 100d
    val zbwSeconds = 200d
    val natProcTimes: Map[String, Double] = Map(
      "GBR" -> gbrSeconds,
      "FRA" -> fraSeconds,
      "ZBW" -> zbwSeconds,
      "ZAR" -> zaSeconds
    )
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(ApiFlightWithSplits(arrival, splits, None), procTimes, natProcTimes, true)
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
          ApiPaxTypeAndQueueCount(PaxTypes.EeaMachineReadable, Queues.EeaDesk, 12, Option(Map("GBR" -> 10, "FRA" -> 2))),
          ApiPaxTypeAndQueueCount(PaxTypes.VisaNational, Queues.NonEeaDesk, 12, Option(Map("ZAR" -> 6, "ZBW" -> 6)))
        ),
        SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        Option(DqEventCodes.DepartureConfirmed),
        PaxNumbers))
    val procTimes = Map(
      PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.EeaDesk) -> 1.5,
      PaxTypeAndQueue(PaxTypes.VisaNational, Queues.NonEeaDesk) -> 5.5
    )
    val gbrSeconds = 45d
    val fraSeconds = 90d
    val zaSeconds = 100d
    val zbwSeconds = 200d
    val natProcTimes: Map[String, Double] = Map(
      "GBR" -> gbrSeconds,
      "FRA" -> fraSeconds,
      "ZBW" -> zbwSeconds,
      "ZAR" -> zaSeconds
    )
    val workloads = WorkloadCalculator
      .flightToFlightSplitMinutes(ApiFlightWithSplits(arrival, splits, None), procTimes, natProcTimes, true)
      .map(m => (m.paxType, m.queueName, m.workLoad))

    val eeaDeskWorkloadInSeconds = (gbrSeconds * 10 + fraSeconds * 2) / 60
    val nonEEADeskWorkloadInSeconds = (zaSeconds * 6 + zbwSeconds * 6) / 60
    workloads === Set(
      (PaxTypes.EeaMachineReadable, Queues.EeaDesk, eeaDeskWorkloadInSeconds * 10 / 12),
      (PaxTypes.VisaNational, Queues.NonEeaDesk, nonEEADeskWorkloadInSeconds * 10 / 12),
      (PaxTypes.EeaMachineReadable, Queues.EeaDesk, eeaDeskWorkloadInSeconds * 2 / 12),
      (PaxTypes.VisaNational, Queues.NonEeaDesk, nonEEADeskWorkloadInSeconds * 2 / 12)
    )
  }

  val terminalQueues: TerminalName => Seq[QueueName] = (t: TerminalName) => Map(
    "T1" -> Seq("Q1", "Q2"),
    "T2" -> Seq("Q1")
  ).getOrElse(t, Seq())

  "Given some loads and affected TQMs from one day at one terminal" >> {
    val day1 = SDate("2019-01-01T12:00").millisSinceEpoch + 180 * Crunch.oneMinuteMillis
    val day2 = SDate("2019-01-02T12:00").millisSinceEpoch + 240 * Crunch.oneMinuteMillis
    val day3 = SDate("2019-01-03T12:00").millisSinceEpoch + 320 * Crunch.oneMinuteMillis
    val loadMinutes = SortedMap[TQM, LoadMinute](
      TQM("T1", "Q1", day1) -> LoadMinute("T1", "Q1", 5, 15, day1),
      TQM("T1", "Q1", day2) -> LoadMinute("T1", "Q1", 6, 10, day2),
      TQM("T1", "Q1", day3) -> LoadMinute("T1", "Q1", 7, 30, day3)
    )

    val affectedTQMs = Seq(TQM("T1", "Q1", day2 + 360 * Crunch.oneMinuteMillis))

    val offsetMillis = 300 * Crunch.oneMinuteMillis

    val result = WorkloadCalculator.daysOfLoadsAffected(loadMinutes, affectedTQMs, offsetMillis, terminalQueues)

    "When I ask for full days of loads for the affected TQMs" >> {
      "I should see 1440 minutes of loads for each queue at that terminal" >> {
        val expectedSize = 1440 * terminalQueues("T1").size

        result.size === expectedSize
      }

      "I should see (1440 * 2) - 1, ie 1 day per queue minus the one defined minute" >> {
        val zeroWorkloadsCount = result.count(_._2.workLoad == 0)

        zeroWorkloadsCount === 1440 + 1439
      }

      "I should see 1 workload of 10 for the one defined minute" >> {
        val nonZeroWorkloads = result.toMap.get(TQM("T1", "Q1", day2)).map(_.workLoad)

        nonZeroWorkloads === Option(10)
      }

      "The first minute of the day should be day 2's local time midnight plus the offset minutes" >> {
        val firstMinuteOfDay = result.map(_._1.minute).min
        val expectedFirstMinute = SDate("2019-01-02T00:00").millisSinceEpoch + offsetMillis

        firstMinuteOfDay === expectedFirstMinute
      }
    }
  }
}
