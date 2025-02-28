package services.graphstages

import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.FlightsApi.FlightsWithSplits
import drt.shared.QueueStatusProviders.QueueStatusProvider
import drt.shared.Queues.{Closed, Open, Queue, QueueFallbacks}
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.api.Arrival
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import services.graphstages.Crunch.{FlightSplitMinute, SplitMinutes}
import services.workloadcalculator.PaxLoadCalculator.Load

import scala.collection.immutable.Map

trait WorkloadCalculatorLike {
  val queueStatusProvider: QueueStatusProvider

  val defaultProcTimes: Map[Terminal, Map[PaxTypeAndQueue, Double]]

  def flightLoadMinutes(flights: FlightsWithSplits): SplitMinutes

  def combineCodeShares(flights: Iterable[ApiFlightWithSplits]): Iterable[ApiFlightWithSplits] = {
    val uniqueFlights: Iterable[ApiFlightWithSplits] = flights
      .toList
      .sortBy(_.apiFlight.ActPax.getOrElse(0))
      .map { fws => (CodeShareKeyOrderedBySchedule(fws), fws) }
      .toMap.values
    uniqueFlights
  }

  def flightsWithPcpWorkload(flights: Iterable[ApiFlightWithSplits]): Iterable[ApiFlightWithSplits] = flights
    .filter { fws =>
      !fws.apiFlight.isCancelled &&
        defaultProcTimes.contains(fws.apiFlight.Terminal) &&
        !fws.apiFlight.Origin.isCta
    }

  def paxTypeAndQueueCountsFromSplits(splitsToUse: Splits): Set[ApiPaxTypeAndQueueCount] = {
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
    splitRatios
  }

}

case class DynamicWorkloadCalculator(defaultProcTimes: Map[Terminal, Map[PaxTypeAndQueue, Double]],
                                     queueStatusProvider: QueueStatusProvider,
                                     fallbacksProvider: QueueFallbacks)
  extends WorkloadCalculatorLike {

  val log: Logger = LoggerFactory.getLogger(getClass)

  override def flightLoadMinutes(flights: FlightsWithSplits): SplitMinutes = {
    val minutes = new SplitMinutes

    flightsWithPcpWorkload(combineCodeShares(flights.flights.values))
      .foreach(incoming => minutes ++= flightToFlightSplitMinutes(incoming))

    minutes
  }

  def flightToFlightSplitMinutes(flightWithSplits: ApiFlightWithSplits): Iterable[FlightSplitMinute] =
    defaultProcTimes.get(flightWithSplits.apiFlight.Terminal) match {
      case None => Iterable()
      case Some(procTimes) =>
        val flight = flightWithSplits.apiFlight

        flightWithSplits.bestSplits.map(splitsToUse => {
          val paxTypeAndQueueCounts = paxTypeAndQueueCountsFromSplits(splitsToUse)

          val paxTypesAndQueuesMinusTransit = paxTypeAndQueueCounts.filterNot(_.queueType == Queues.Transfer)

          flight.paxDeparturesByMinute(20)
            .flatMap {
              case (minuteMillis, flightPaxInMinute) =>
                paxTypesAndQueuesMinusTransit
                  .map { case ptqc@ApiPaxTypeAndQueueCount(pt, queue, _, _, _) =>
                    def findAlternativeQueue(originalQueue: Queue, queuesToTry: Iterable[Queue]): Queue = {
                      queuesToTry.find(queueStatusProvider.statusAt(flight.Terminal, _, SDate(minuteMillis).getHours()) == Open) match {
                        case Some(queue) => queue
                        case None =>
                          log.error(s"Failed to find alternative queue for $pt ($queuesToTry). Reverting to $originalQueue")
                          originalQueue
                      }
                    }

                    val finalPtqc = queueStatusProvider.statusAt(flight.Terminal, queue, SDate(minuteMillis).getHours()) match {
                      case Closed =>
                        val fallbacks = fallbacksProvider.availableFallbacks(flight.Terminal, queue, pt)
                        val newQueue = findAlternativeQueue(queue, fallbacks)
                        ptqc.copy(queueType = newQueue)
                      case Open =>
                        ptqc
                    }

                    flightSplitMinute(flight, procTimes, minuteMillis, flightPaxInMinute, finalPtqc)
                  }
            }
        }).getOrElse(Seq())
    }


  def flightSplitMinute(arrival: Arrival,
                        procTimes: Map[PaxTypeAndQueue, Load],
                        minuteMillis: MillisSinceEpoch,
                        flightPaxInMinute: Int,
                        apiSplitRatio: ApiPaxTypeAndQueueCount
                       ): FlightSplitMinute = {
    val splitPaxInMinute = apiSplitRatio.paxCount * flightPaxInMinute
    val paxTypeQueueProcTime = procTimes.getOrElse(PaxTypeAndQueue(apiSplitRatio.passengerType, apiSplitRatio.queueType), 0d)
    val defaultWorkload = splitPaxInMinute * paxTypeQueueProcTime

    FlightSplitMinute(CodeShareKeyOrderedBySchedule(arrival), apiSplitRatio.passengerType, arrival.Terminal, apiSplitRatio.queueType, splitPaxInMinute, defaultWorkload, minuteMillis)
  }
}
