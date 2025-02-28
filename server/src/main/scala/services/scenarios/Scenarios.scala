package services.scenarios

import actors.GetState
import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.util.Timeout
import drt.shared.CrunchApi.DeskRecMinutes
import drt.shared.FlightsApi.FlightsWithSplits
import drt.shared.api.Arrival
import drt.shared.{AirportConfig, PcpPax, SimulationParams}
import manifests.queues.SplitsCalculator
import passengersplits.parsing.VoyageManifestParser
import services.crunch.desklimits.PortDeskLimits
import services.crunch.deskrecs.DynamicRunnableDeskRecs.HistoricManifestsProvider
import services.crunch.deskrecs.RunnableOptimisation.CrunchRequest
import services.crunch.deskrecs.{DynamicRunnableDeskRecs, PortDesksAndWaitsProvider, RunnableOptimisation}
import services.{OptimiserWithFlexibleProcessors, SDate}

import scala.concurrent.{ExecutionContextExecutor, Future}

object Scenarios {

  def simulationResult(
                        simulationParams: SimulationParams,
                        simulationAirportConfig: AirportConfig,
                        splitsCalculator: SplitsCalculator,
                        flightsProvider: CrunchRequest => Future[Source[List[Arrival], NotUsed]],
                        liveManifestsProvider: CrunchRequest => Future[Source[VoyageManifestParser.VoyageManifests, NotUsed]],
                        historicManifestsProvider: HistoricManifestsProvider,
                        flightsActor: ActorRef,
                        portStateActor: ActorRef
                      )(implicit system: ActorSystem, timeout: Timeout): Future[DeskRecMinutes] = {


    implicit val ec: ExecutionContextExecutor = system.dispatcher
    implicit val mat = ActorMaterializer.create(system)
    val portDesksAndWaitsProvider: PortDesksAndWaitsProvider = PortDesksAndWaitsProvider(simulationAirportConfig, OptimiserWithFlexibleProcessors.crunch)
    val terminalDeskLimits = PortDeskLimits.fixed(simulationAirportConfig)


    val deskRecsProducer = DynamicRunnableDeskRecs.crunchRequestsToQueueMinutes(
      flightsProvider,
      liveManifestsProvider,
      historicManifestsProvider,
      splitsCalculator,
      flightsActor,
      portDesksAndWaitsProvider.flightsToLoads,
      portDesksAndWaitsProvider.loadsToDesks,
      terminalDeskLimits)

    val (crunchRequestQueue, deskRecsKillSwitch) = RunnableOptimisation.createGraph(portStateActor, deskRecsProducer).run()

    crunchRequestQueue.offer(CrunchRequest(SDate(simulationParams.date).millisSinceEpoch, simulationAirportConfig.crunchOffsetMinutes, simulationAirportConfig.minutesToCrunch))

    val futureDeskRecMinutes: Future[DeskRecMinutes] = (portStateActor ? GetState).map {
      case drm: DeskRecMinutes => DeskRecMinutes(drm.minutes.filter(_.terminal == simulationParams.terminal))
    }
    futureDeskRecMinutes.onComplete(_ => deskRecsKillSwitch.shutdown())
    futureDeskRecMinutes
  }


}
