package services.graphstages

import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.FlightsApi.{QueueName, TerminalName}
import drt.shared.SplitRatiosNs.SplitSources
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import services.{PcpArrival, SDate}
import services.graphstages.Crunch.{FlightSplitMinute, LoadMinute}
import services.workloadcalculator.PaxLoadCalculator.{Load, minutesForHours, paxDeparturesPerMinutes, paxOffFlowRate}

import scala.collection.immutable.{Map, SortedMap}

object WorkloadCalculator {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def flightToFlightSplitMinutes(flightWithSplits: ApiFlightWithSplits,
                                 procTimes: Map[PaxTypeAndQueue, Double],
                                 nationalityProcessingTimes: Map[String, Double],
                                 useNationalityBasedProcTimes: Boolean
                                ): Set[FlightSplitMinute] = {
    val flight = flightWithSplits.apiFlight
    val splitsToUseOption = flightWithSplits.bestSplits

    splitsToUseOption.map(splitsToUse => {
      val totalPax = splitsToUse.splitStyle match {
        case UndefinedSplitStyle => 0
        case _ => ArrivalHelper.bestPax(flight)
      }
      val splitRatios: Set[ApiPaxTypeAndQueueCount] = splitsToUse.splitStyle match {
        case UndefinedSplitStyle => splitsToUse.splits.map(qc => qc.copy(paxCount = 0))
        case PaxNumbers =>
          val splitsWithoutTransit = splitsToUse.splits.filter(_.queueType != Queues.Transfer)
          val totalSplitsPax = splitsWithoutTransit.toList.map(_.paxCount).sum
          splitsWithoutTransit.map(qc => qc.copy(paxCount = qc.paxCount / totalSplitsPax))
        case _ => splitsToUse.splits.map(qc => qc.copy(paxCount = qc.paxCount / 100))
      }

      val splitsWithoutTransit = splitRatios.filterNot(_.queueType == Queues.Transfer)

      val totalPaxWithNationality = splitsWithoutTransit.toList.flatMap(_.nationalities.map(_.values.sum)).sum

      val startMinute: Long = PcpArrival.timeToNearestMinute(flight.PcpTime.getOrElse(0))
      minutesForHours(startMinute, 1)
        .zip(paxDeparturesPerMinutes(totalPax.toInt, paxOffFlowRate))
        .flatMap {
          case (minuteMillis, flightPaxInMinute) =>
            splitsWithoutTransit
              .map(apiSplit => {
                flightSplitMinute(
                  flight,
                  procTimes,
                  minuteMillis,
                  flightPaxInMinute,
                  apiSplit,
                  Percentage,
                  nationalityProcessingTimes,
                  totalPaxWithNationality,
                  useNationalityBasedProcTimes
                )
              })
        }.toSet
    }).getOrElse(Set())
  }

  def flightSplitMinute(flight: Arrival,
                        procTimes: Map[PaxTypeAndQueue, Load],
                        minuteMillis: MillisSinceEpoch,
                        flightPaxInMinute: Int,
                        apiSplitRatio: ApiPaxTypeAndQueueCount,
                        splitStyle: SplitStyle,
                        nationalityProcessingTimes: Map[String, Double],
                        totalPaxWithNationality: Double,
                        useNationalityBasedProcTimes: Boolean
                       ): FlightSplitMinute = {
    val splitPaxInMinute = apiSplitRatio.paxCount * flightPaxInMinute
    val paxTypeQueueProcTime = procTimes.getOrElse(PaxTypeAndQueue(apiSplitRatio.passengerType, apiSplitRatio.queueType), 0d)
    val defaultWorkload = splitPaxInMinute * paxTypeQueueProcTime

    val splitWorkLoadInMinute = (apiSplitRatio.nationalities, useNationalityBasedProcTimes) match {
      case (Some(nats), true) if nats.values.sum > 0 =>
        val bestPax = ArrivalHelper.bestPax(flight)
        val natsToPaxRatio = totalPaxWithNationality / bestPax
        val natFactor = (flightPaxInMinute.toDouble / bestPax) / natsToPaxRatio
        log.debug(s"totalNats: $totalPaxWithNationality / bestPax: $bestPax, natFactor: $natFactor - ($flightPaxInMinute / $bestPax) / $natsToPaxRatio")
        val natBasedWorkload = nats
          .map {
            case (nat, pax) => nationalityProcessingTimes.get(nat) match {
              case Some(procTime) =>
                val procTimeInMinutes = procTime / 60
                log.debug(s"Using processing time for $pax $nat: $procTimeInMinutes (rather than $paxTypeQueueProcTime)")
                pax * procTimeInMinutes * natFactor
              case None =>
                log.debug(s"Processing time for $nat not found. Using ${apiSplitRatio.passengerType} -> ${apiSplitRatio.queueType}: $paxTypeQueueProcTime")
                pax * paxTypeQueueProcTime
            }
          }
          .sum
        log.debug(s"Nationality based workload: $natBasedWorkload vs $defaultWorkload default workload")
        natBasedWorkload
      case _ =>
        defaultWorkload
    }
    FlightSplitMinute(flight.uniqueId, apiSplitRatio.passengerType, flight.Terminal, apiSplitRatio.queueType, splitPaxInMinute, splitWorkLoadInMinute, minuteMillis)
  }

  def daysOfLoadsAffected(loadMinutes: SortedMap[TQM, LoadMinute], affectedTqms: Iterable[TQM], dayOffsetMillis: Long, terminalQueues: TerminalName => Seq[QueueName]): Seq[(TQM, LoadMinute)] = {
    val uniqueMinutes: Set[MillisSinceEpoch] = affectedTqms.map(_.minute).toSet
    val daysAffected = uniqueMinutes.map(m => SDate(m).getLocalPreviousMidnight.millisSinceEpoch + dayOffsetMillis).toSeq
    val affectedTerminals = affectedTqms.map(_.terminalName).toSet.toSeq
    val daysAffectedFullWorkloads: Seq[(TQM, LoadMinute)] = for {
      dayStartMillis <- daysAffected
      t <- affectedTerminals
      q <- terminalQueues(t)
      minOfDay <- 0L until Crunch.oneDayMillis by Crunch.oneMinuteMillis
    } yield {
      val m = dayStartMillis + minOfDay
      val tqm = TQM(t, q, m)
      val lm = loadMinutes.getOrElse(tqm, LoadMinute(terminalName = t, queueName = q, paxLoad = 0, workLoad = 0, minute = m))
      (tqm, lm)
    }
    daysAffectedFullWorkloads
  }
}
