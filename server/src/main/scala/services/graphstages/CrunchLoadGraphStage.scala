package services.graphstages

import akka.stream._
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import drt.shared.CrunchApi._
import drt.shared.FlightsApi.{QueueName, TerminalName}
import drt.shared._
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import services.crunch.{OptimiserLike, WorkloadToOptimise}
import services.graphstages.Crunch._

import scala.Option
import scala.collection.immutable.{Map, SortedMap}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps


class CrunchLoadGraphStage(name: String = "",
                           optionalInitialCrunchMinutes: Option[CrunchMinutes],
                           airportConfig: AirportConfig,
                           expireAfterMillis: MillisSinceEpoch,
                           now: () => SDateLike,
                           optimiser: OptimiserLike,
                           crunchPeriodStartMillis: SDateLike => SDateLike,
                           minutesToCrunch: Int)
  extends GraphStage[FlowShape[Loads, DeskRecMinutes]] {

  val inLoads: Inlet[Loads] = Inlet[Loads]("inLoads.in")
  val outDeskRecMinutes: Outlet[DeskRecMinutes] = Outlet[DeskRecMinutes]("outDeskRecMinutes.out")

  override val shape = new FlowShape(inLoads, outDeskRecMinutes)

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) {
    var loadMinutes: SortedMap[TQM, LoadMinute] = SortedMap()
    var existingDeskRecMinutes: SortedMap[TQM, DeskRecMinute] = SortedMap()
    var deskRecMinutesToPush: Map[TQM, DeskRecMinute] = Map()

    val log: Logger = LoggerFactory.getLogger(s"$getClass-$name")

    override def preStart(): Unit = {
      loadMinutes = optionalInitialCrunchMinutes match {
        case None => SortedMap[TQM, LoadMinute]()
        case Some(CrunchMinutes(cms)) => SortedMap[TQM, LoadMinute]() ++ cms.map(cm => {
          val lm = LoadMinute(cm.terminalName, cm.queueName, cm.paxLoad, cm.workLoad, cm.minute)
          (lm.key, lm)
        })
      }

      super.preStart()
    }

    setHandler(inLoads, new InHandler {
      override def onPush(): Unit = {
        val start = SDate.now()
        val incomingLoads = grab(inLoads)
        log.info(s"Received ${incomingLoads.loadMinutes.size} loads")

        val allMinuteMillis = incomingLoads.loadMinutes.keys.map(_.minute)
        val firstMinute = crunchPeriodStartMillis(SDate(allMinuteMillis.min))
        val lastMinute = firstMinute.addMinutes(minutesToCrunch)
        log.info(s"Crunch ${firstMinute.toLocalDateTimeString()} - ${lastMinute.toLocalDateTimeString()}")
        val affectedTerminals = incomingLoads.loadMinutes.keys.map(_.terminalName).toSet

        val updatedLoads = loadMinutes ++ incomingLoads.loadMinutes
        loadMinutes = purgeExpired(updatedLoads, now, expireAfterMillis.toInt)

        val deskRecMinutes = crunchLoads(firstMinute.millisSinceEpoch, lastMinute.millisSinceEpoch, affectedTerminals)

        val affectedDeskRecs = deskRecMinutes.foldLeft(Map[TQM, DeskRecMinute]()) {
          case (soFar, (key, drm)) =>
            existingDeskRecMinutes.get(key) match {
              case Some(existingDrm) if existingDrm == drm => soFar
              case _ => soFar.updated(key, drm)
            }
        }

        val updatedDeskRecs = existingDeskRecMinutes ++ deskRecMinutes

        existingDeskRecMinutes = purgeExpired(updatedDeskRecs, now, expireAfterMillis.toInt)
        deskRecMinutesToPush = deskRecMinutesToPush ++ affectedDeskRecs

        log.info(s"Now have ${deskRecMinutesToPush.size} desk rec minutes to push")

        pushStateIfReady()

        pullLoads()
        log.info(s"inLoads Took ${SDate.now().millisSinceEpoch - start.millisSinceEpoch}ms")
      }
    })

    def crunchLoads(firstMinute: MillisSinceEpoch, lastMinute: MillisSinceEpoch, terminalsToCrunch: Set[TerminalName]): SortedMap[TQM, DeskRecMinute] = {
      val terminalQueueDeskRecs = for {
        terminal <- terminalsToCrunch
        queue <- airportConfig.nonTransferQueues(terminal)
      } yield {
        val lms = (firstMinute until lastMinute by 60000).map(minute =>
          loadMinutes.getOrElse(TQM(terminal, queue, minute), LoadMinute(terminal, queue, 0, 0, minute)))
        crunchQueue(firstMinute, lastMinute, terminal, queue, lms)
      }
      SortedMap[TQM, DeskRecMinute]() ++ terminalQueueDeskRecs.toSeq.flatten
    }

    def crunchQueue(firstMinute: MillisSinceEpoch, lastMinute: MillisSinceEpoch, tn: TerminalName, qn: QueueName, qLms: IndexedSeq[LoadMinute]): SortedMap[TQM, DeskRecMinute] = {
      val sla = airportConfig.slaByQueue.getOrElse(qn, 15)
      val paxMinutes = qLms.map(_.paxLoad)
      val workMinutes = qLms.map(_.workLoad)
      val adjustedWorkMinutes = if (qn == Queues.EGate) workMinutes.map(_ / airportConfig.eGateBankSize) else workMinutes
      val minuteMillis: Seq[MillisSinceEpoch] = firstMinute until lastMinute by 60000
      val (minDesks, maxDesks) = minMaxDesksForQueue(minuteMillis, tn, qn)
      val description = s"${airportConfig.portCode}/$tn/$qn/${SDate(firstMinute).toISOString()}"
      val workloadToOptimise = WorkloadToOptimise(adjustedWorkMinutes, minDesks, maxDesks, sla, description)

      Await.result(optimiser.requestDesksAndWaits(workloadToOptimise), 120 seconds) match {
        case None =>
          log.warn(s"Did not receive a crunch result")
          SortedMap[TQM, DeskRecMinute]()
        case Some(daw) =>
          SortedMap[TQM, DeskRecMinute]() ++ minuteMillis.zipWithIndex.map {
            case (minute, idx) =>
              val wl = workMinutes(idx)
              val pl = paxMinutes(idx)
              val drm = DeskRecMinute(tn, qn, minute, pl, wl, daw.desks(idx), daw.waits(idx))
              (drm.key, drm)
          }
      }
    }

    def filterTerminalQueueMinutes[A <: TerminalQueueMinute](firstMinute: MillisSinceEpoch, lastMinute: MillisSinceEpoch, terminalsToUpdate: Set[TerminalName], thingsToFilter: SortedMap[TQM, A]): Set[A] = {
      val maybeThings = for {
        terminalName <- terminalsToUpdate
        queueName <- airportConfig.queues.getOrElse(terminalName, Seq())
        minute <- firstMinute until lastMinute by oneMinuteMillis
      } yield {
        val key = MinuteHelper.key(terminalName, queueName, minute)
        thingsToFilter.get(key)
      }

      maybeThings.collect { case Some(thing) => thing }
    }

    def minMaxDesksForQueue(deskRecMinutes: Seq[MillisSinceEpoch], tn: TerminalName, qn: QueueName): (Seq[Int], Seq[Int]) = {
      val defaultMinMaxDesks = (Seq.fill(24)(0), Seq.fill(24)(10))
      val queueMinMaxDesks = airportConfig.minMaxDesksByTerminalQueue.getOrElse(tn, Map()).getOrElse(qn, defaultMinMaxDesks)
      val minDesks = deskRecMinutes.map(desksForHourOfDayInUKLocalTime(_, queueMinMaxDesks._1))
      val maxDesks = deskRecMinutes.map(desksForHourOfDayInUKLocalTime(_, queueMinMaxDesks._2))
      (minDesks, maxDesks)
    }

    setHandler(outDeskRecMinutes, new OutHandler {
      override def onPull(): Unit = {
        val start = SDate.now()
        pushStateIfReady()
        pullLoads()
        log.info(s"outDeskRecMinutes Took ${SDate.now().millisSinceEpoch - start.millisSinceEpoch}ms")
      }
    })

    def pullLoads(): Unit = {
      if (!hasBeenPulled(inLoads)) {
        log.info(s"Pulling inFlightsWithSplits")
        pull(inLoads)
      }
    }

    def pushStateIfReady(): Unit = {
      if (deskRecMinutesToPush.isEmpty) log.info(s"We have no crunch minutes. Nothing to push")
      else if (isAvailable(outDeskRecMinutes)) {
        log.info(s"Pushing ${deskRecMinutesToPush.size} crunch minutes")
        push(outDeskRecMinutes, DeskRecMinutes(deskRecMinutesToPush.values.toSeq))
        deskRecMinutesToPush = Map()
      } else log.info(s"outDeskRecMinutes not available to push")
    }
  }
}

case class DeskRecMinute(terminalName: TerminalName,
                         queueName: QueueName,
                         minute: MillisSinceEpoch,
                         paxLoad: Double,
                         workLoad: Double,
                         deskRec: Int,
                         waitTime: Int) extends DeskRecMinuteLike {
  lazy val key: TQM = MinuteHelper.key(terminalName, queueName, minute)
}

case class DeskRecMinutes(minutes: Seq[DeskRecMinute]) extends PortStateMinutes {
  def applyTo(maybePortState: Option[PortState], now: MillisSinceEpoch): (PortState, PortStateDiff) = {
    val portState = maybePortState match {
      case None => PortState.empty
      case Some(ps) => ps
    }

    val crunchMinutesDiff = minutes.foldLeft(List[(TQM, CrunchMinute)]()) {
      case (diffSoFar, updatedDrm) =>
        val maybeMinute: Option[CrunchMinute] = portState.crunchMinutes.get(updatedDrm.key)
        val mergedCm: CrunchMinute = mergeMinute(maybeMinute, updatedDrm, now)
        (mergedCm.key, mergedCm) :: diffSoFar
    }

    val newPortState = portState.copy(crunchMinutes = portState.crunchMinutes ++ crunchMinutesDiff.toMap)
    val newDiff = PortStateDiff(Seq(), SortedMap[Int, ApiFlightWithSplits](), SortedMap[TQM, CrunchMinute]() ++ crunchMinutesDiff, SortedMap[TM, StaffMinute]())

    (newPortState, newDiff)
  }

  def newCrunchMinutes: SortedMap[TQM, CrunchMinute] = SortedMap[TQM, CrunchMinute]() ++ minutes
    .map(CrunchMinute(_))
    .map(cm => (cm.key, cm))
    .toMap

  def mergeMinute(maybeMinute: Option[CrunchMinute], updatedDrm: DeskRecMinute, now: MillisSinceEpoch): CrunchMinute = maybeMinute
    .map(_.copy(
      paxLoad = updatedDrm.paxLoad,
      workLoad = updatedDrm.workLoad,
      deskRec = updatedDrm.deskRec,
      waitTime = updatedDrm.waitTime
    ))
    .getOrElse(CrunchMinute(updatedDrm))
    .copy(lastUpdated = Option(now))
}

case class OptimiserService(host: String, port: String) {
  val uri: String = s"http://$host:$port/optimise"
}
