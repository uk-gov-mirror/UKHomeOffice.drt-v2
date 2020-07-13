package services.graphstages

import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.FlightsApi.FlightsWithSplits
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.api.Arrival
import org.slf4j.{Logger, LoggerFactory}
import services.PcpArrival
import services.graphstages.Crunch.{FlightSplitMinute, SplitMinutes}
import services.workloadcalculator.PaxLoadCalculator.{Load, minutesForHours, paxDeparturesPerMinutes, paxOffFlowRate}

import scala.collection.immutable.Map

object WorkloadCalculator {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def flightLoadMinutes(
                         flights: FlightsWithSplits,
                         defaultProcTimes: Map[Terminal, Map[PaxTypeAndQueue, Double]],
                         pcpPaxFn: Arrival => Int
                       ): SplitMinutes = {
    val uniqueFlights: Iterable[ApiFlightWithSplits] = flights
      .flights.toMap.values.toList
      .sortBy(_.apiFlight.actPax.getOrElse(0))
      .map { fws => (CodeShareKeyOrderedBySchedule(fws), fws) }
      .toMap.values

    val minutes = new SplitMinutes

    uniqueFlights
      .filter(fws => !fws.apiFlight.isCancelled && defaultProcTimes.contains(fws.apiFlight.terminal))
      .foreach { incoming =>
        val procTimes = defaultProcTimes(incoming.apiFlight.terminal)
        val flightMinutes = flightToFlightSplitMinutes(
          incoming,
          procTimes,
          Map(),
          useNationalityBasedProcTimes = false,
          pcpPaxFn
        )
        minutes ++= flightMinutes
      }

    minutes
  }

  def flightToFlightSplitMinutes(flightWithSplits: ApiFlightWithSplits,
                                 procTimes: Map[PaxTypeAndQueue, Double],
                                 nationalityProcessingTimes: Map[Nationality, Double],
                                 useNationalityBasedProcTimes: Boolean,
                                 pcpPaxFn: Arrival => Int
                                ): Seq[FlightSplitMinute] = {
    val flight = flightWithSplits.apiFlight
    val splitsToUseOption = flightWithSplits.bestSplits

    splitsToUseOption.map(splitsToUse => {
      val totalPax = splitsToUse.splitStyle match {
        case UndefinedSplitStyle => 0
        case _ => pcpPaxFn(flight)
      }
      val splitRatios: Set[ApiPaxTypeAndQueueCount] = splitsToUse.splitStyle match {
        case UndefinedSplitStyle => splitsToUse.splits.map(qc => qc.copy(paxCount = 0))
        case PaxNumbers =>
          val splitsWithoutTransit = splitsToUse.splits.filter(_.queueType != Queues.Transfer)
          val totalSplitsPax: Load = splitsWithoutTransit.toList.map(_.paxCount).sum
          if (totalSplitsPax == 0.0)
            splitsWithoutTransit
          else
            splitsWithoutTransit.map(qc => qc.copy(paxCount = qc.paxCount / totalSplitsPax))
        case _ => splitsToUse.splits.map(qc => qc.copy(paxCount = qc.paxCount / 100))
      }

      val splitsWithoutTransit = splitRatios.filterNot(_.queueType == Queues.Transfer)

      val totalPaxWithNationality = splitsWithoutTransit.toList.flatMap(_.nationalities.map(_.values.sum)).sum

      val startMinute: Long = PcpArrival.timeToNearestMinute(flight.pcpTime.getOrElse(0))
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
                  nationalityProcessingTimes,
                  totalPaxWithNationality,
                  useNationalityBasedProcTimes,
                  pcpPaxFn
                )
              })
        }
    }).getOrElse(Seq())
  }

  def flightSplitMinute(arrival: Arrival,
                        procTimes: Map[PaxTypeAndQueue, Load],
                        minuteMillis: MillisSinceEpoch,
                        flightPaxInMinute: Int,
                        apiSplitRatio: ApiPaxTypeAndQueueCount,
                        nationalityProcessingTimes: Map[Nationality, Double],
                        totalPaxWithNationality: Double,
                        useNationalityBasedProcTimes: Boolean,
                        pcpPaxFn: Arrival => Int
                       ): FlightSplitMinute = {
    val splitPaxInMinute = apiSplitRatio.paxCount * flightPaxInMinute
    val paxTypeQueueProcTime = procTimes.getOrElse(PaxTypeAndQueue(apiSplitRatio.passengerType, apiSplitRatio.queueType), 0d)
    val defaultWorkload = splitPaxInMinute * paxTypeQueueProcTime

    val splitWorkLoadInMinute = (apiSplitRatio.nationalities, useNationalityBasedProcTimes) match {
      case (Some(nats), true) if nats.values.sum > 0 =>
        val bestPax = pcpPaxFn(arrival)
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
    FlightSplitMinute(CodeShareKeyOrderedBySchedule(arrival), apiSplitRatio.passengerType, arrival.terminal, apiSplitRatio.queueType, splitPaxInMinute, splitWorkLoadInMinute, minuteMillis)
  }
}
