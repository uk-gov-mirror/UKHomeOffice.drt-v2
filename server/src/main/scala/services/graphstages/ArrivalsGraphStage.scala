package services.graphstages

import actors.Ports
import akka.stream._
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import drt.shared.Terminals.{InvalidTerminal, Terminal}
import drt.shared._
import drt.shared.api.Arrival
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import services.arrivals.{ArrivalDataSanitiser, LiveArrivalsUtil}
import services.metrics.{Metrics, StageTimer}

import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.postfixOps

sealed trait ArrivalsSourceType

case object LiveArrivals extends ArrivalsSourceType

case object LiveBaseArrivals extends ArrivalsSourceType

case object ForecastArrivals extends ArrivalsSourceType

case object BaseArrivals extends ArrivalsSourceType

class ArrivalsGraphStage(name: String = "",
                         initialForecastBaseArrivals: mutable.SortedMap[UniqueArrival, Arrival],
                         initialForecastArrivals: mutable.SortedMap[UniqueArrival, Arrival],
                         initialLiveBaseArrivals: mutable.SortedMap[UniqueArrival, Arrival],
                         initialLiveArrivals: mutable.SortedMap[UniqueArrival, Arrival],
                         initialMergedArrivals: mutable.SortedMap[UniqueArrival, Arrival],
                         pcpArrivalTime: Arrival => MilliDate,
                         validPortTerminals: Set[Terminal],
                         arrivalDataSanitiser: ArrivalDataSanitiser,
                         expireAfterMillis: Int,
                         now: () => SDateLike)
  extends GraphStage[FanInShape4[List[Arrival], List[Arrival], List[Arrival], List[Arrival], ArrivalsDiff]] {

  val inForecastBaseArrivals: Inlet[List[Arrival]] = Inlet[List[Arrival]]("FlightsForecastBase.in")
  val inForecastArrivals: Inlet[List[Arrival]] = Inlet[List[Arrival]]("FlightsForecast.in")
  val inLiveBaseArrivals: Inlet[List[Arrival]] = Inlet[List[Arrival]]("FlightsLiveBase.in")
  val inLiveArrivals: Inlet[List[Arrival]] = Inlet[List[Arrival]]("FlightsLive.in")
  val outArrivalsDiff: Outlet[ArrivalsDiff] = Outlet[ArrivalsDiff]("ArrivalsDiff.out")
  override val shape = new FanInShape4(inForecastBaseArrivals, inForecastArrivals, inLiveBaseArrivals, inLiveArrivals, outArrivalsDiff)
  val stageName = "arrivals"

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) {
    val forecastBaseArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap()
    val forecastArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap()
    val liveBaseArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap()
    val liveArrivals: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap()
    val merged: mutable.SortedMap[UniqueArrival, Arrival] = mutable.SortedMap()
    var toPush: Option[ArrivalsDiff] = None

    val log: Logger = LoggerFactory.getLogger(s"$getClass-$name")

    override def preStart(): Unit = {
      log.info(s"Received ${initialForecastBaseArrivals.size} initial base arrivals")
      forecastBaseArrivals ++= relevantFlights(SortedMap[UniqueArrival, Arrival]() ++ initialForecastBaseArrivals)
      log.info(s"Received ${initialForecastArrivals.size} initial forecast arrivals")
      prepInitialArrivals(initialForecastArrivals, forecastArrivals)

      log.info(s"Received ${initialLiveBaseArrivals.size} initial live base arrivals")
      prepInitialArrivals(initialLiveBaseArrivals, liveBaseArrivals)
      log.info(s"Received ${initialLiveArrivals.size} initial live arrivals")
      prepInitialArrivals(initialLiveArrivals, liveArrivals)

      initialMergedArrivals.map { case (_, a) => merged += (UniqueArrival(a) -> a) }
      super.preStart()
    }

    def prepInitialArrivals(initialArrivals: mutable.SortedMap[UniqueArrival, Arrival],
                            arrivals: mutable.SortedMap[UniqueArrival, Arrival]): Unit = {
      arrivals ++= relevantFlights(SortedMap[UniqueArrival, Arrival]() ++ initialArrivals)
      Crunch.purgeExpired(arrivals, UniqueArrival.atTime, now, expireAfterMillis.toInt)
    }

    setHandler(inForecastBaseArrivals, new InHandler {
      override def onPush(): Unit = onPushArrivals(inForecastBaseArrivals, BaseArrivals)
    })

    setHandler(inForecastArrivals, new InHandler {
      override def onPush(): Unit = onPushArrivals(inForecastArrivals, ForecastArrivals)
    })

    setHandler(inLiveBaseArrivals, new InHandler {
      override def onPush(): Unit = onPushArrivals(inLiveBaseArrivals, LiveBaseArrivals)
    })

    setHandler(inLiveArrivals, new InHandler {
      override def onPush(): Unit = onPushArrivals(inLiveArrivals, LiveArrivals)
    })

    setHandler(outArrivalsDiff, new OutHandler {
      override def onPull(): Unit = {
        val timer = StageTimer(stageName, outArrivalsDiff)
        pushIfAvailable(toPush, outArrivalsDiff)

        List(inLiveBaseArrivals, inLiveArrivals, inForecastArrivals, inForecastBaseArrivals).foreach(inlet => if (!hasBeenPulled(inlet)) {
          log.debug(s"Pulling Inlet: ${inlet.toString()}")
          pull(inlet)
        })
        timer.stopAndReport()
      }
    })

    def onPushArrivals(arrivalsInlet: Inlet[List[Arrival]], sourceType: ArrivalsSourceType): Unit = {
      val timer = StageTimer(stageName, outArrivalsDiff)

      val incoming = grab(arrivalsInlet)

      log.info(s"Grabbed ${incoming.length} arrivals from $arrivalsInlet of $sourceType")
      if (incoming.nonEmpty || sourceType == BaseArrivals) handleIncomingArrivals(sourceType, incoming)

      if (!hasBeenPulled(arrivalsInlet)) pull(arrivalsInlet)

      timer.stopAndReport()
    }

    def handleIncomingArrivals(sourceType: ArrivalsSourceType, incomingArrivals: Seq[Arrival]): Unit = {
      val filteredArrivals = relevantFlights(SortedMap[UniqueArrival, Arrival]() ++ incomingArrivals.map(a => (UniqueArrival(a), a)))
      log.info(s"${filteredArrivals.size} arrivals after filtering")
      sourceType match {
        case LiveArrivals =>
          updateArrivalsSource(liveArrivals, filteredArrivals)
          toPush = mergeUpdatesFromKeys(liveArrivals.keys)
        case LiveBaseArrivals =>
          updateArrivalsSource(liveBaseArrivals, filteredArrivals)
          val missingTerminals = liveBaseArrivals.count {
            case (_, a) if a.terminal == InvalidTerminal => true
            case _ => false
          }
          log.info(s"Got $missingTerminals Cirium Arrivals with no terminal")
          toPush = mergeUpdatesFromKeys(liveBaseArrivals.keys)
        case ForecastArrivals =>
          updateArrivalsSource(forecastArrivals, filteredArrivals)
          toPush = mergeUpdatesFromKeys(forecastArrivals.keys)
        case BaseArrivals =>
          forecastBaseArrivals.clear
          forecastBaseArrivals ++= filteredArrivals
          toPush = mergeUpdatesFromAllSources()
      }
      pushIfAvailable(toPush, outArrivalsDiff)
    }

    def updateArrivalsSource(existingArrivals: mutable.SortedMap[UniqueArrival, Arrival],
                             newArrivals: SortedMap[UniqueArrival, Arrival]): Unit = newArrivals.foreach {
      case (key, newArrival) =>
        if (!existingArrivals.contains(key) || !existingArrivals(key).equals(newArrival)) existingArrivals += (key -> newArrival)
    }

    def mergeUpdatesFromAllSources(): Option[ArrivalsDiff] = maybeDiffFromAllSources().map(diff => {
      merged --= diff.toRemove.map(UniqueArrival(_))
      merged ++= diff.toUpdate
      diff
    })

    def mergeUpdatesFromKeys(uniqueArrivals: Iterable[UniqueArrival]): Option[ArrivalsDiff] = {
      val updatedArrivals = getUpdatesFromNonBaseArrivals(uniqueArrivals)

      merged ++= updatedArrivals

      updateDiffToPush(updatedArrivals)
    }

    def updateDiffToPush(updatedLiveArrivals: SortedMap[UniqueArrival, Arrival]): Option[ArrivalsDiff] = {
      toPush match {
        case None => Option(ArrivalsDiff(SortedMap[UniqueArrival, Arrival]() ++ updatedLiveArrivals, Set()))
        case Some(diff) =>
          val newToUpdate = updatedLiveArrivals.foldLeft(diff.toUpdate) {
            case (toUpdateSoFar, (ak, arrival)) => toUpdateSoFar.updated(ak, arrival)
          }
          Option(diff.copy(toUpdate = newToUpdate))
      }
    }

    def maybeDiffFromAllSources(): Option[ArrivalsDiff] = {
      purgeExpired()

      val existingArrivalsKeys = merged.keys.toSet
      val newArrivalsKeys = forecastBaseArrivals.keys.toSet ++ liveArrivals.keys.toSet
      val arrivalsWithUpdates = getUpdatesFromBaseArrivals

      val removedArrivals = (existingArrivalsKeys -- newArrivalsKeys).map(merged(_))

      arrivalsWithUpdates.foreach { case (ak, mergedArrival) =>
        merged += (ak -> mergedArrival)
      }

      if (arrivalsWithUpdates.nonEmpty || removedArrivals.nonEmpty) {
        val updates = SortedMap[UniqueArrival, Arrival]() ++ arrivalsWithUpdates
        Option(ArrivalsDiff(updates, removedArrivals))
      }
      else None
    }

    def purgeExpired(): Unit = {
      Crunch.purgeExpired(liveArrivals, UniqueArrival.atTime, now, expireAfterMillis.toInt)
      Crunch.purgeExpired(liveBaseArrivals, UniqueArrival.atTime, now, expireAfterMillis.toInt)
      Crunch.purgeExpired(forecastArrivals, UniqueArrival.atTime, now, expireAfterMillis.toInt)
      Crunch.purgeExpired(forecastBaseArrivals, UniqueArrival.atTime, now, expireAfterMillis.toInt)
      Crunch.purgeExpired(merged, UniqueArrival.atTime, now, expireAfterMillis.toInt)
    }

    def relevantFlights(arrivals: SortedMap[UniqueArrival, Arrival]): SortedMap[UniqueArrival, Arrival] = {
      val toRemove = arrivals.filter {
        case (_, f) if !isFlightRelevant(f) =>
          log.debug(s"Filtering out irrelevant arrival: ${f.flightCode}, ${SDate(f.scheduled).toISOString()}, ${f.origin}")
          true
        case _ => false
      }.keys

      val minusRemovals = arrivals -- toRemove

      minusRemovals
    }

    def isFlightRelevant(flight: Arrival): Boolean = {
      val isValidSuffix = !flight.flightCodeSuffix.exists(fcs => fcs.suffix == "P" || fcs.suffix == "F")
      validPortTerminals.contains(flight.terminal) && !Ports.domesticPorts.contains(flight.origin) && isValidSuffix
    }

    def pushIfAvailable(arrivalsToPush: Option[ArrivalsDiff], outlet: Outlet[ArrivalsDiff]): Unit = {
      if (isAvailable(outlet)) {
        arrivalsToPush.foreach { diff =>
          Metrics.counter(s"$stageName.arrivals.updates", diff.toUpdate.size)
          Metrics.counter(s"$stageName.arrivals.removals", diff.toRemove.size)
          push(outArrivalsDiff, diff)
          toPush = None
        }
      } else log.debug(s"outMerged not available to push")
    }

    def getUpdatesFromBaseArrivals: mutable.SortedMap[UniqueArrival, Arrival] = {
      val arrivals = mutable.SortedMap[UniqueArrival, Arrival]()
      forecastBaseArrivals.foreach {
        case (key, baseArrival) =>
          val mergedArrival = mergeBaseArrival(baseArrival)
          if (arrivalHasUpdates(merged.get(key), mergedArrival)) arrivals += (key -> mergedArrival)
      }
      arrivals
    }

    def getUpdatesFromNonBaseArrivals(keys: Iterable[UniqueArrival]): SortedMap[UniqueArrival, Arrival] = SortedMap[UniqueArrival, Arrival]() ++ keys
      .foldLeft(Map[UniqueArrival, Arrival]()) {
        case (updatedArrivalsSoFar, key) =>
          mergeArrival(key) match {
            case Some(mergedArrival) =>
              if (arrivalHasUpdates(merged.get(key), mergedArrival)) updatedArrivalsSoFar.updated(key, mergedArrival) else updatedArrivalsSoFar
            case None => updatedArrivalsSoFar
          }
      }

    def arrivalHasUpdates(maybeExistingArrival: Option[Arrival], updatedArrival: Arrival): Boolean = {
      maybeExistingArrival.isEmpty || !maybeExistingArrival.get.equals(updatedArrival)
    }

    def mergeBaseArrival(baseArrival: Arrival): Arrival = {
      val key = UniqueArrival(baseArrival)
      mergeBestFieldsFromSources(baseArrival, mergeArrival(key).getOrElse(baseArrival))
    }

    def mergeArrival(key: UniqueArrival): Option[Arrival] = {
      val maybeLiveBaseArrivalWithSanitisedData = liveBaseArrivals.get(key).map(arrivalDataSanitiser.withSaneEstimates)
      val maybeBestArrival: Option[Arrival] = (
        liveArrivals.get(key),
        maybeLiveBaseArrivalWithSanitisedData) match {
        case (Some(liveArrival), None) => Option(liveArrival)
        case (Some(liveArrival), Some(baseLiveArrival)) =>
          val mergedLiveArrival = LiveArrivalsUtil.mergePortFeedWithBase(liveArrival, baseLiveArrival)
          val sanitisedLiveArrival = ArrivalDataSanitiser
            .arrivalDataSanitiserWithoutThresholds
            .withSaneEstimates(mergedLiveArrival)
          Option(sanitisedLiveArrival)
        case (None, Some(baseLiveArrival)) if forecastBaseArrivals.contains(key) =>

          Option(baseLiveArrival)
        case _ => forecastBaseArrivals.get(key)
      }
      maybeBestArrival.map(bestArrival => {
        val arrivalForFlightCode = forecastBaseArrivals.getOrElse(key, bestArrival)
        mergeBestFieldsFromSources(arrivalForFlightCode, bestArrival)
      })
    }

    def mergeBestFieldsFromSources(baseArrival: Arrival, bestArrival: Arrival): Arrival = {
      val key = UniqueArrival(baseArrival)
      val (pax, transPax) = bestPaxNos(key)
      bestArrival.copy(
        carrierCode = baseArrival.carrierCode,
        voyageNumber = baseArrival.voyageNumber,
        actPax = pax,
        tranPax = transPax,
        status = bestStatus(key),
        feedSources = feedSources(key),
        pcpTime = Option(pcpArrivalTime(bestArrival).millisSinceEpoch)
        )
    }

    def trustLiveDataThreshold: FiniteDuration = 3 hours

    def bestPaxNos(key: UniqueArrival): (Option[Int], Option[Int]) =
      (liveArrivals.get(key), forecastArrivals.get(key), forecastBaseArrivals.get(key)) match {
        case (Some(live), _, _) if paxDefined(live) => (live.actPax, live.tranPax)
        case (_, Some(fcst), _) if paxDefined(fcst) => (fcst.actPax, fcst.tranPax)
        case (_, _, Some(base)) if paxDefined(base) => (base.actPax, base.tranPax)
        case _ => (None, None)
      }

    def bestStatus(key: UniqueArrival): ArrivalStatus =
      (liveArrivals.get(key), liveBaseArrivals.get(key), forecastArrivals.get(key), forecastBaseArrivals.get(key)) match {
        case (Some(live), Some(liveBase), _, _) if live.status.description == "UNK" => liveBase.status
        case (Some(live), _, _, _) => live.status
        case (_, Some(liveBase), _, _) => liveBase.status
        case (_, _, Some(forecast), _) => forecast.status
        case (_, _, _, Some(forecastBase)) => forecastBase.status
        case _ => ArrivalStatus("Unknown")
      }

    def feedSources(uniqueArrival: UniqueArrival): Set[FeedSource] = {
      List(
        liveArrivals.get(uniqueArrival).map(_ => LiveFeedSource),
        forecastArrivals.get(uniqueArrival).map(_ => ForecastFeedSource),
        forecastBaseArrivals.get(uniqueArrival).map(_ => AclFeedSource),
        liveBaseArrivals.get(uniqueArrival).map(_ => LiveBaseFeedSource)
        ).flatten.toSet
    }
  }

  private def paxDefined(baseArrival: Arrival): Boolean = baseArrival.actPax.isDefined
}
