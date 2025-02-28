package test

import actors._
import actors.acking.AckingReceiver.Ack
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Cancellable, Props, Status}
import akka.pattern.ask
import akka.persistence.inmemory.extension.{InMemoryJournalStorage, InMemorySnapshotStorage, StorageExtension}
import akka.stream.scaladsl.Source
import akka.stream.{KillSwitch, Materializer}
import akka.util.Timeout
import drt.shared._
import manifests.passengers.BestAvailableManifest
import manifests.{ManifestLookupLike, UniqueArrivalKey}
import play.api.Configuration
import play.api.mvc.{Headers, Session}
import server.feeds.ArrivalsFeedResponse
import services.SDate
import test.TestActors.{TestStaffMovementsActor, _}
import test.feeds.test.{CSVFixtures, TestArrivalsActor, TestFixtureFeed, TestManifestsActor}
import test.roles.TestUserRoleProvider
import uk.gov.homeoffice.drt.auth.Roles.Role

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.language.postfixOps
import scala.util.Success

case class MockManifestLookupService(implicit ec: ExecutionContext) extends ManifestLookupLike {
  override def maybeBestAvailableManifest(arrivalPort: PortCode,
                                          departurePort: PortCode,
                                          voyageNumber: VoyageNumber,
                                          scheduled: SDateLike)
                                         (implicit mat: Materializer): Future[(UniqueArrivalKey, Option[BestAvailableManifest])] =
    Future((UniqueArrivalKey(arrivalPort, departurePort, voyageNumber, scheduled), None))
}

case class TestDrtSystem(config: Configuration, airportConfig: AirportConfig)
                        (implicit val materializer: Materializer,
                         val ec: ExecutionContext,
                         val system: ActorSystem) extends DrtSystemInterface {

  import DrtStaticParameters._

  log.warn("Using test System")

  override val baseArrivalsActor: ActorRef = system.actorOf(Props(new TestForecastBaseArrivalsActor(now, expireAfterMillis)), name = "base-arrivals-actor")
  override val forecastArrivalsActor: ActorRef = system.actorOf(Props(new TestForecastPortArrivalsActor(now, expireAfterMillis)), name = "forecast-arrivals-actor")
  override val liveArrivalsActor: ActorRef = system.actorOf(Props(new TestLiveArrivalsActor(now, expireAfterMillis)), name = "live-arrivals-actor")

  val manifestLookups: ManifestLookups = ManifestLookups(system)

  override val shiftsActor: ActorRef = system.actorOf(Props(new TestShiftsActor(now, timeBeforeThisMonth(now))))
  override val fixedPointsActor: ActorRef = system.actorOf(Props(new TestFixedPointsActor(now)))
  override val staffMovementsActor: ActorRef = system.actorOf(Props(new TestStaffMovementsActor(now, time48HoursAgo(now))), "TestActor-StaffMovements")
  override val aggregatedArrivalsActor: ActorRef = system.actorOf(Props(new MockAggregatedArrivalsActor()))
  override val crunchQueueActor: ActorRef = system.actorOf(Props(new TestCrunchQueueActor(now, airportConfig.crunchOffsetMinutes, airportConfig.minutesToCrunch)), name = "crunch-queue-actor")
  override val deploymentQueueActor: ActorRef = system.actorOf(Props(new TestDeploymentQueueActor(now, airportConfig.crunchOffsetMinutes, airportConfig.minutesToCrunch)), name = "staff-queue-actor")
  override val manifestsRouterActor: ActorRef = system.actorOf(Props(new TestVoyageManifestsActor(manifestLookups.manifestsByDayLookup, manifestLookups.updateManifests, crunchQueueActor)), name = "voyage-manifests-router-actor")

  override val manifestLookupService: ManifestLookupLike = MockManifestLookupService()
  override val minuteLookups: MinuteLookupsLike = TestMinuteLookups(system, now, MilliTimes.oneDayMillis, airportConfig.queuesByTerminal, deploymentQueueActor)
  val flightLookups: TestFlightLookups = TestFlightLookups(system, now, airportConfig.queuesByTerminal, crunchQueueActor)
  override val flightsActor: ActorRef = flightLookups.flightsActor
  override val queuesActor: ActorRef = minuteLookups.queueMinutesActor
  override val staffActor: ActorRef = minuteLookups.staffMinutesActor
  override val queueUpdates: ActorRef = system.actorOf(Props(
    new QueueTestUpdatesSupervisor(
      now,
      airportConfig.queuesByTerminal.keys.toList,
      PartitionedPortStateActor.queueUpdatesProps(now, journalType)
    )),
    "updates-supervisor-queues")
  override val staffUpdates: ActorRef = system.actorOf(Props(
    new StaffTestUpdatesSupervisor(
      now,
      airportConfig.queuesByTerminal.keys.toList,
      PartitionedPortStateActor.staffUpdatesProps(now, journalType)
    )
  ), "updates-supervisor-staff")
  override val flightUpdates: ActorRef = system.actorOf(Props(
    new TestFlightUpdatesSupervisor(
      now,
      airportConfig.queuesByTerminal.keys.toList,
      PartitionedPortStateActor.flightUpdatesProps(now, journalType)
    )
  ), "updates-supervisor-flight")

  override val portStateActor: ActorRef = system.actorOf(
    Props(
      new TestPartitionedPortStateActor(
        flightsActor,
        queuesActor,
        staffActor,
        queueUpdates,
        staffUpdates,
        flightUpdates,
        now,
        airportConfig.queuesByTerminal,
        journalType
      )
    )
  )

  val testManifestsActor: ActorRef = system.actorOf(Props(new TestManifestsActor()), s"TestActor-APIManifests")
  val testArrivalActor: ActorRef = system.actorOf(Props(new TestArrivalsActor()), s"TestActor-LiveArrivals")
  val testFeed: Source[ArrivalsFeedResponse, Cancellable] = TestFixtureFeed(system, testArrivalActor)

  val testActors = List(
    baseArrivalsActor,
    forecastArrivalsActor,
    liveArrivalsActor,
    forecastArrivalsActor,
    portStateActor,
    manifestsRouterActor,
    shiftsActor,
    fixedPointsActor,
    staffMovementsActor,
    aggregatedArrivalsActor,
    testManifestsActor,
    testArrivalActor,
    crunchQueueActor,
    deploymentQueueActor
  )

  val restartActor: ActorRef = system.actorOf(Props(new RestartActor(startSystem, testActors)), name = "TestActor-ResetData")

  config.getOptional[String]("test.live_fixture_csv").foreach { file =>
    implicit val timeout: Timeout = Timeout(250 milliseconds)
    log.info(s"Loading fixtures from $file")
    val testActor = system.actorSelection(s"akka://${airportConfig.portCode.iata.toLowerCase}-drt-actor-system/user/TestActor-LiveArrivals").resolveOne()
    system.scheduler.schedule(1 second, 1 day)({
      val day = SDate.now().toISODateOnly
      CSVFixtures.csvPathToArrivalsOnDate(day, file).collect {
        case Success(arrival) =>
          testActor.map(_ ! arrival)
      }
    })
  }

  override def liveArrivalsSource(portCode: PortCode): Source[ArrivalsFeedResponse, Cancellable] = testFeed

  override def getRoles(config: Configuration,
                        headers: Headers,
                        session: Session): Set[Role] = TestUserRoleProvider.getRoles(config, headers, session)

  override def run(): Unit = {
    restartActor ! StartTestSystem
  }

  def startSystem: () => List[KillSwitch] = () => {
    val cs = startCrunchSystem(
      initialPortState = None,
      initialForecastBaseArrivals = None,
      initialForecastArrivals = None,
      initialLiveBaseArrivals = None,
      initialLiveArrivals = None,
      refreshArrivalsOnStart = false,
      refreshManifestsOnStart = false,
      startDeskRecs = startDeskRecs)

    subscribeStaffingActors(cs)
    startScheduledFeedImports(cs)

    testManifestsActor ! SubscribeResponseQueue(cs.manifestsLiveResponse)

    cs.killSwitches
  }
}

class RestartActor(startSystem: () => List[KillSwitch],
                   testActors: List[ActorRef]) extends Actor with ActorLogging {

  var currentKillSwitches: List[KillSwitch] = List()

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  override def receive: Receive = {
    case ResetData =>
      val replyTo = sender()

      resetInMemoryData()

      log.info(s"About to shut down everything. Pressing kill switches")

      currentKillSwitches.zipWithIndex.foreach { case (ks, idx) =>
        log.info(s"Kill switch ${idx + 1}")
        ks.shutdown()
      }

      Future.sequence(testActors.map(_.ask(ResetData)(new Timeout(5 second)))).onComplete { _ =>
        log.info(s"Shutdown triggered")
        startTestSystem()
        replyTo ! Ack
      }

    case Status.Success(_) =>
      log.info(s"Got a Status acknowledgement from InMemoryJournalStorage")

    case Status.Failure(t) =>
      log.error("Got a failure message", t)

    case StartTestSystem => startTestSystem()
    case u => log.error(s"Received unexpected message: ${u.getClass}")
  }

  def startTestSystem(): Unit = currentKillSwitches = startSystem()

  def resetInMemoryData(): Unit = {
    StorageExtension(context.system).journalStorage ! InMemoryJournalStorage.ClearJournal
    StorageExtension(context.system).snapshotStorage ! InMemorySnapshotStorage.ClearSnapshots
  }
}

case object StartTestSystem
