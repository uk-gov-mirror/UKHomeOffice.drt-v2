package controllers

import java.nio.ByteBuffer
import java.util.{Calendar, TimeZone, UUID}

import javax.inject.{Inject, Singleton}
import actors._
import actors.pointInTime.CrunchStateReadActor
import akka.actor._
import akka.event.{Logging, LoggingAdapter}
import akka.pattern.{AskableActorRef, _}
import akka.stream._
import akka.util.{ByteString, Timeout}
import api.{KeyCloakAuth, KeyCloakAuthError, KeyCloakAuthResponse, KeyCloakAuthToken}
import boopickle.CompositePickler
import boopickle.Default._
import buildinfo.BuildInfo
import com.typesafe.config.ConfigFactory
import drt.http.ProdSendAndReceive
import drt.shared.CrunchApi.{groupCrunchMinutesByX, _}
import drt.shared.FlightsApi.TerminalName
import drt.shared.KeyCloakApi.{KeyCloakGroup, KeyCloakUser}
import drt.shared.SplitRatiosNs.SplitRatios
import drt.shared.{AirportConfig, Api, Arrival, _}
import drt.splits.DrtSplits
import drt.staff.ImportStaff
import drt.users.{KeyCloakClient, KeyCloakGroups}
import org.joda.time.chrono.ISOChronology
import org.slf4j.{Logger, LoggerFactory}
import play.api.http.{HeaderNames, HttpEntity}
import play.api.libs.json._
import play.api.mvc._
import play.api.{Configuration, Environment}
import server.feeds.acl.AclFeed
import services.PcpArrival.{pcpFrom, _}
import services.SplitsProvider.SplitProvider
import services._
import services.graphstages.Crunch
import services.graphstages.Crunch._
import services.staffing.StaffTimeSlots
import services.workloadcalculator.PaxLoadCalculator
import services.workloadcalculator.PaxLoadCalculator.PaxTypeAndQueueCount
import test.TestDrtSystem

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try

object Router extends autowire.Server[ByteBuffer, Pickler, Pickler] {

  import scala.language.experimental.macros

  override def read[R: Pickler](p: ByteBuffer): R = Unpickle[R].fromBytes(p)

  def myroute[Trait](target: Trait): Router = macro MyMacros.routeMacro[Trait, ByteBuffer]

  override def write[R: Pickler](r: R): ByteBuffer = Pickle.intoBytes(r)
}

object PaxFlow {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def makeFlightPaxFlowCalculator(splitRatioForFlight: Arrival => Option[SplitRatios],
                                  bestPax: Arrival => Int): Arrival => IndexedSeq[(MillisSinceEpoch, PaxTypeAndQueueCount)] = {
    val provider = PaxLoadCalculator.flightPaxFlowProvider(splitRatioForFlight, bestPax)
    arrival => {
      val pax = bestPax(arrival)
      val paxFlow = provider(arrival)
      val summedPax = paxFlow.map(_._2.paxSum).sum
      val firstPaxTime = paxFlow.headOption.map(pf => SDate(pf._1).toString)
      log.debug(s"${Arrival.summaryString(arrival)} pax: $pax, summedFlowPax: $summedPax, deltaPax: ${pax - summedPax}, firstPaxTime: $firstPaxTime")
      paxFlow
    }
  }

  def splitRatioForFlight(splitsProviders: List[SplitProvider])
                         (flight: Arrival): Option[SplitRatios] = SplitsProvider.splitsForFlight(splitsProviders)(flight)

  def pcpArrivalTimeForFlight(timeToChoxMillis: MillisSinceEpoch, firstPaxOffMillis: MillisSinceEpoch)
                             (walkTimeProvider: FlightWalkTime)
                             (flight: Arrival): MilliDate = pcpFrom(timeToChoxMillis, firstPaxOffMillis, walkTimeProvider)(flight)
}

trait AirportConfiguration {
  def airportConfig: AirportConfig
}

trait AirportConfProvider extends AirportConfiguration {
  val portCode: String = ConfigFactory.load().getString("portcode").toUpperCase
  val config: Configuration

  def mockProd: String = sys.env.getOrElse("MOCK_PROD", "PROD").toUpperCase

  def useStaffingInput: Boolean = config.getOptional[String]("feature-flags.use-v2-staff-input").isDefined

  def contactEmail: Option[String] = config.getOptional[String]("contact-email")

  def getPortConfFromEnvVar: AirportConfig = AirportConfigs.confByPort(portCode)

  def airportConfig: AirportConfig = getPortConfFromEnvVar.copy(
    useStaffingInput = useStaffingInput,
    contactEmail = contactEmail
  )
}

trait ProdPassengerSplitProviders {
  self: AirportConfiguration =>

  val csvSplitsProvider: SplitsProvider.SplitProvider = SplitsProvider.csvProvider

  def egatePercentageProvider(apiFlight: Arrival): Double = {
    CSVPassengerSplitsProvider.egatePercentageFromSplit(csvSplitsProvider(apiFlight.IATA, MilliDate(apiFlight.Scheduled)), 0.6)
  }

  def fastTrackPercentageProvider(apiFlight: Arrival): Option[FastTrackPercentages] =
    Option(CSVPassengerSplitsProvider.fastTrackPercentagesFromSplit(csvSplitsProvider(apiFlight.IATA, MilliDate(apiFlight.Scheduled)), 0d, 0d))

  private implicit val timeout: Timeout = Timeout(250 milliseconds)
}

trait ImplicitTimeoutProvider {
  implicit val timeout: Timeout = Timeout(1 second)
}

trait UserRoleProviderLike {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def userRolesFromHeader(headers: Headers): Set[Role] = headers.get("X-Auth-Roles").map(_.split(",").flatMap(Roles.parse).toSet).getOrElse(Set.empty[Role])

  def getRoles(config: Configuration, headers: Headers, session: Session): Set[Role]

  def getLoggedInUser(config: Configuration, headers: Headers, session: Session): LoggedInUser = {
    LoggedInUser(
      userName = headers.get("X-Auth-Username").getOrElse("Unknown"),
      id = headers.get("X-Auth-Userid").getOrElse("Unknown"),
      email = headers.get("X-Auth-Email").getOrElse("Unknown"),
      roles = getRoles(config, headers, session)
    )
  }
}

@Singleton
class Application @Inject()(implicit val config: Configuration,
                            implicit val mat: Materializer,
                            env: Environment,
                            val system: ActorSystem,
                            ec: ExecutionContext)
  extends InjectedController
    with AirportConfProvider
    with ApplicationWithAlerts
    with ApplicationWithImports
    with ProdPassengerSplitProviders
    with ImplicitTimeoutProvider {

  val googleTrackingCode: String = config.get[String]("googleTrackingCode")

  val ctrl: DrtSystemInterface = config.getOptional[String]("env") match {
    case Some("test") =>
      new TestDrtSystem(system, config, getPortConfFromEnvVar)
    case _ =>
      DrtSystem(system, config, getPortConfFromEnvVar)
  }
  ctrl.run()

  val virusScannerUrl: String = config.get[String]("virus-scanner-url")

  val virusScanner: VirusScanner = VirusScanner(VirusScanService(virusScannerUrl))

  def log: LoggingAdapter = system.log

  log.info(s"Starting DRTv2 build ${BuildInfo.version}")

  log.info(s"ISOChronology.getInstance: ${ISOChronology.getInstance}")

  def defaultTimeZone: String = TimeZone.getDefault.getID

  def systemTimeZone: String = System.getProperty("user.timezone")

  log.info(s"System.getProperty(user.timezone): $systemTimeZone")
  log.info(s"TimeZone.getDefault: $defaultTimeZone")
  assert(systemTimeZone == "UTC", "System Timezone is not set to UTC")
  assert(defaultTimeZone == "UTC", "Default Timezone is not set to UTC")

  log.info(s"timezone: ${Calendar.getInstance().getTimeZone()}")

  log.info(s"Application using airportConfig $airportConfig")

  val cacheActorRef: AskableActorRef = system.actorOf(Props(classOf[CachingCrunchReadActor]), name = "cache-actor")

  def previousDay(date: MilliDate): SDateLike = {
    val oneDayInMillis = 60 * 60 * 24 * 1000L
    SDate(date.millisSinceEpoch - oneDayInMillis)
  }

  val permissionDeniedMessage = "You do not have permission manage users"

  object ApiService {
    def apply(
               airportConfig: AirportConfig,
               shiftsActor: ActorRef,
               fixedPointsActor: ActorRef,
               staffMovementsActor: ActorRef,
               headers: Headers,
               session: Session
             ): ApiService = new ApiService(airportConfig, shiftsActor, fixedPointsActor, staffMovementsActor, headers, session) {

      override implicit val timeout: Timeout = Timeout(5 seconds)

      DrtSplits()
      def actorSystem: ActorSystem = system

      def getCrunchStateForDay(day: MillisSinceEpoch): Future[Either[CrunchStateError, Option[CrunchState]]] = loadBestCrunchStateForPointInTime(day)

      def getApplicationVersion(): String = BuildInfo.version

      override def getCrunchStateForPointInTime(pointInTime: MillisSinceEpoch): Future[Either[CrunchStateError, Option[CrunchState]]] = crunchStateAtPointInTime(pointInTime)

      def getCrunchUpdates(sinceMillis: MillisSinceEpoch,
                           windowStartMillis: MillisSinceEpoch,
                           windowEndMillis: MillisSinceEpoch): Future[Either[CrunchStateError, Option[CrunchUpdates]]] = {
        val liveStateCutOff = getLocalNextMidnight(ctrl.now()).addDays(1).millisSinceEpoch

        val stateActor = if (windowStartMillis < liveStateCutOff) liveCrunchStateActor else forecastCrunchStateActor

        val crunchStateFuture = stateActor.ask(GetUpdatesSince(sinceMillis, windowStartMillis, windowEndMillis))(new Timeout(30 seconds))

        crunchStateFuture.map {
          case Some(cu: CrunchUpdates) => Right(Option(cu))
          case _ => Right(None)
        } recover {
          case t =>
            log.warn(s"Didn't get a CrunchUpdates: $t")
            Left(CrunchStateError(t.getMessage))
        }
      }

      def isLoggedIn(): Boolean = {
        true
      }

      def getLoggedInUser(): LoggedInUser = ctrl.getLoggedInUser(config, headers, session)

      def getFeedStatuses(): Future[Seq[FeedStatuses]] = ctrl.getFeedStatus

      def forecastWeekSummary(startDay: MillisSinceEpoch,
                              terminal: TerminalName): Future[Option[ForecastPeriodWithHeadlines]] = {
        val startOfWeekMidnight = getLocalLastMidnight(SDate(startDay))
        val endOfForecast = startOfWeekMidnight.addDays(7).millisSinceEpoch
        val now = SDate.now()

        val startOfForecast = if (startOfWeekMidnight.millisSinceEpoch < now.millisSinceEpoch) {
          log.info(s"${startOfWeekMidnight.toLocalDateTimeString()} < ${now.toLocalDateTimeString()}, going to use ${getLocalNextMidnight(now)} instead")
          getLocalNextMidnight(now)
        } else startOfWeekMidnight

        val crunchStateFuture = forecastCrunchStateActor.ask(
          GetPortState(startOfForecast.millisSinceEpoch, endOfForecast)
        )(new Timeout(30 seconds))

        crunchStateFuture.map {
          case Some(PortState(_, m, s)) =>
            log.info(s"Sent forecast for week beginning ${SDate(startDay).toISOString()} on $terminal")
            val timeSlotsByDay = Forecast.rollUpForWeek(m.values.toSet, s.values.toSet, terminal)
            val period = ForecastPeriod(timeSlotsByDay)
            val headlineFigures = Forecast.headLineFigures(m.values.toSet, terminal)
            Option(ForecastPeriodWithHeadlines(period, headlineFigures))
          case None =>
            log.info(s"No forecast available for week beginning ${SDate(startDay).toISOString()} on $terminal")
            None
        }
      }

      def forecastWeekHeadlineFigures(startDay: MillisSinceEpoch,
                                      terminal: TerminalName): Future[Option[ForecastHeadlineFigures]] = {
        val midnight = getLocalLastMidnight(SDate(startDay))
        val crunchStateFuture = forecastCrunchStateActor.ask(
          GetPortState(midnight.millisSinceEpoch, midnight.addDays(7).millisSinceEpoch)
        )(new Timeout(30 seconds))

        crunchStateFuture.map {
          case Some(PortState(_, m, _)) =>

            Option(Forecast.headLineFigures(m.values.toSet, terminal))
          case None =>
            log.info(s"No forecast available for week beginning ${SDate(startDay).toISOString()} on $terminal")
            None
        }
      }

      def updateShifts(shiftsToUpdate: Seq[StaffAssignment]): Unit = {
        if (getLoggedInUser().roles.contains(StaffEdit)) {
          log.info(s"Saving ${shiftsToUpdate.length} shift staff assignments")
          shiftsActor ! UpdateShifts(shiftsToUpdate)
        } else throw new Exception("You do not have permission to edit staffing.")
      }

      def getShiftsForMonth(month: MillisSinceEpoch, terminalName: TerminalName): Future[ShiftAssignments] = {
        val shiftsFuture = shiftsActor ? GetState

        shiftsFuture.collect {
          case shifts: ShiftAssignments =>
            log.info(s"Shifts: Retrieved shifts from actor for month starting: ${SDate(month).toISOString()}")
            val monthInLocalTime = SDate(month, Crunch.europeLondonTimeZone)
            StaffTimeSlots.getShiftsForMonth(shifts, monthInLocalTime, terminalName)
        }
      }

      def keyCloakClient: KeyCloakClient with ProdSendAndReceive = {
        val token = headers.get("X-Auth-Token").getOrElse(throw new Exception("X-Auth-Token missing from headers, we need this to query the Key Cloak API."))
        val keyCloakUrl = config.getOptional[String]("key-cloak.url").getOrElse(throw new Exception("Missing key-cloak.url config value, we need this to query the Key Cloak API"))
        new KeyCloakClient(token, keyCloakUrl, actorSystem) with ProdSendAndReceive
      }

      def getKeyCloakUsers(): Future[List[KeyCloakUser]] = {
        log.info(s"Got these roles: ${getLoggedInUser().roles}")
        if (getLoggedInUser().roles.contains(ManageUsers)) {
          Future(keyCloakClient.getAllUsers().toList)
        } else throw new Exception(permissionDeniedMessage)
      }

      def getKeyCloakGroups(): Future[List[KeyCloakGroup]] = {
        if (getLoggedInUser().roles.contains(ManageUsers)) {
          keyCloakClient.getGroups()
        } else throw new Exception(permissionDeniedMessage)
      }

      def getKeyCloakUserGroups(userId: UUID): Future[Set[KeyCloakGroup]] = {
        if (getLoggedInUser().roles.contains(ManageUsers)) {
          keyCloakClient.getUserGroups(userId).map(_.toSet)
        } else throw new Exception(permissionDeniedMessage)
      }

      case class KeyCloakGroups(groups: List[KeyCloakGroup])


      def addUserToGroups(userId: UUID, groups: Set[String]): Future[Unit] =
        if (getLoggedInUser().roles.contains(ManageUsers)) {
          val futureGroupIds: Future[KeyCloakGroups] = keyCloakClient
            .getGroups()
            .map(kcGroups => KeyCloakGroups(kcGroups.filter(g => groups.contains(g.name))))


          futureGroupIds.map {
            case KeyCloakGroups(groups) if groups.nonEmpty =>
              log.info(s"Adding ${groups.map(_.name)} to $userId")
              groups.map(group => {
                val response = keyCloakClient.addUserToGroup(userId, group.id)
                response.map(res => log.info(s"Added group and got: ${res.status}  $res")
                )
              })
            case _ => log.error(s"Unable to add $userId to $groups")
          }
        } else throw new Exception(permissionDeniedMessage)

      def removeUserFromGroups(userId: UUID, groups: Set[String]): Future[Unit] =
        keyCloakClient
          .getGroups()
          .map(kcGroups => kcGroups.filter(g => groups.contains(g.name))
            .map(g => keyCloakClient.removeUserFromGroup(userId, g.id)))

      def getAlerts(createdAfter: MillisSinceEpoch): Future[Seq[Alert]] = {
        for {
          alerts <- (ctrl.alertsActor ? GetState).mapTo[Seq[Alert]]
        } yield alerts.filter(a => a.createdAt > createdAfter)
      }

      def deleteAllAlerts(): Unit = ctrl.alertsActor ? DeleteAlerts

      def saveAlert(alert: Alert): Unit = ctrl.alertsActor ? alert

      override def liveCrunchStateActor: AskableActorRef = ctrl.liveCrunchStateActor

      override def forecastCrunchStateActor: AskableActorRef = ctrl.forecastCrunchStateActor

    }
  }

  def loadBestCrunchStateForPointInTime(day: MillisSinceEpoch): Future[Either[CrunchStateError, Option[CrunchState]]] =
    if (isHistoricDate(day)) {
      crunchStateForEndOfDay(day)
    } else if (day <= getLocalNextMidnight(SDate.now()).millisSinceEpoch) {
      ctrl.liveCrunchStateActor.ask(GetState).map {
        case Some(PortState(f, m, s)) => Right(Option(CrunchState(f.values.toSet, m.values.toSet, s.values.toSet)))
        case _ => Right(None)
      }
    } else {
      crunchStateForDayInForecast(day)
    }

  def crunchStateForDayInForecast(day: MillisSinceEpoch): Future[Either[CrunchStateError, Option[CrunchState]]] = {
    val firstMinute = getLocalLastMidnight(SDate(day)).millisSinceEpoch
    val lastMinute = SDate(firstMinute).addHours(airportConfig.dayLengthHours).millisSinceEpoch

    val crunchStateFuture = ctrl.forecastCrunchStateActor.ask(GetPortState(firstMinute, lastMinute))(new Timeout(30 seconds))

    crunchStateFuture.map {
      case Some(PortState(f, m, s)) => Right(Option(CrunchState(f.values.toSet, m.values.toSet, s.values.toSet)))
      case _ => Right(None)
    } recover {
      case t =>
        log.warning(s"Didn't get a CrunchState: $t")
        Left(CrunchStateError(t.getMessage))
    }
  }

  def isHistoricDate(day: MillisSinceEpoch): Boolean = day < getLocalLastMidnight(SDate.now()).millisSinceEpoch

  def index = Action {
    Ok(views.html.index("DRT - BorderForce", portCode, googleTrackingCode))
  }

  def getLoggedInUser(): Action[AnyContent] = Action { request =>
    val user = ctrl.getLoggedInUser(config, request.headers, request.session)

    implicit val userWrites = new Writes[LoggedInUser] {
      def writes(user: LoggedInUser) = Json.obj(
        "userName" -> user.userName,
        "id" -> user.id,
        "email" -> user.email,
        "roles" -> user.roles.map(_.name)
      )
    }

    Ok(Json.toJson(user))
  }

  def getShouldReload(): Action[AnyContent] = Action { request =>

    val shouldRedirect: Boolean = config.getOptional[Boolean]("feature-flags.acp-redirect").getOrElse(false)
    Ok(Json.obj("reload" -> shouldRedirect))
  }

  def apiLogin(): Action[Map[String, Seq[String]]] = Action.async(parse.tolerantFormUrlEncoded) { request =>

    def postStringValOrElse(key: String): Option[String] = {
      request.body.get(key).map(_.head)
    }

    val tokenUrlOption = config.getOptional[String]("key-cloak.token_url")
    val clientIdOption = config.getOptional[String]("key-cloak.client_id")
    val clientSecretOption = config.getOptional[String]("key-cloak.client_secret")
    val usernameOption = postStringValOrElse("username")
    val passwordOption = postStringValOrElse("password")
    import api.KeyCloakAuthTokenParserProtocol._
    import spray.json._

    def tokenToHttpResponse(username: String)(token: KeyCloakAuthResponse) = {

      token match {
        case t: KeyCloakAuthToken =>
          log.info(s"Successful login to API via keycloak for $username")
          Ok(t.toJson.toString)
        case e: KeyCloakAuthError =>
          log.info(s"Failed login to API via keycloak for $username")
          BadRequest(e.toJson.toString)
      }
    }

    def missingPostFieldsResponse = Future(
      BadRequest(KeyCloakAuthError("invalid_form_data", "You must provide a username and password").toJson.toString)
    )

    val result: Option[Future[Result]] = for {
      tokenUrl <- tokenUrlOption
      clientId <- clientIdOption
      clientSecret <- clientSecretOption
    } yield (usernameOption, passwordOption) match {
      case (Some(username), Some(password)) =>
        val authClient = new KeyCloakAuth(tokenUrl, clientId, clientSecret, system) with ProdSendAndReceive
        authClient.getToken(username, password).map(tokenToHttpResponse(username))
      case _ =>
        log.info(s"Invalid post fields for api login.")
        missingPostFieldsResponse
    }

    def disabledFeatureResponse = Future(NotImplemented(
      KeyCloakAuthError(
        "feature_not_implemented",
        "This feature is not currently available for this port on DRT"
      ).toJson.toString
    ))

    result match {
      case Some(f) => f.map(t => t)
      case None =>
        disabledFeatureResponse
    }
  }

  def getUserHasPortAccess(): Action[AnyContent] = auth {
    Action {
      Ok("{userHasAccess: true}")
    }
  }

  def keyCloakClient(headers: Headers): KeyCloakClient with ProdSendAndReceive = {
    val token = headers.get("X-Auth-Token").getOrElse(throw new Exception("X-Auth-Token missing from headers, we need this to query the Key Cloak API."))
    val keyCloakUrl = config.getOptional[String]("key-cloak.url").getOrElse(throw new Exception("Missing key-cloak.url config value, we need this to query the Key Cloak API"))
    new KeyCloakClient(token, keyCloakUrl, system) with ProdSendAndReceive
  }

  def exportUsers(): Action[AnyContent] = Action.async { request =>
    val loggedInUser = ctrl.getLoggedInUser(config, request.headers, request.session)

    if (loggedInUser.roles.contains(ManageUsers)) {
      val client = keyCloakClient(request.headers)
      client
        .getGroups()
        .flatMap(groupList => KeyCloakGroups(groupList, client).usersWithGroupsCsvContent)
        .map(csvContent => Result(
          ResponseHeader(200, Map("Content-Disposition" -> s"attachment; filename='users-with-groups.csv'")),
          HttpEntity.Strict(ByteString(csvContent), Option("application/csv"))
        ))
    } else throw new Exception(permissionDeniedMessage)
  }

  def crunchStateAtPointInTime(pointInTime: MillisSinceEpoch): Future[Either[CrunchStateError, Option[CrunchState]]] = {
    val relativeLastMidnight = getLocalLastMidnight(SDate(pointInTime)).millisSinceEpoch
    val startMillis = relativeLastMidnight
    val endMillis = relativeLastMidnight + oneHourMillis * airportConfig.dayLengthHours

    portStatePeriodAtPointInTime(startMillis, endMillis, pointInTime)
  }

  def crunchStateForEndOfDay(day: MillisSinceEpoch): Future[Either[CrunchStateError, Option[CrunchState]]] = {
    val relativeLastMidnight = getLocalLastMidnight(SDate(day)).millisSinceEpoch
    val startMillis = relativeLastMidnight
    val endMillis = relativeLastMidnight + oneHourMillis * airportConfig.dayLengthHours
    val pointInTime = startMillis + oneDayMillis + oneHourMillis * 3

    portStatePeriodAtPointInTime(startMillis, endMillis, pointInTime)
  }

  def portStatePeriodAtPointInTime(startMillis: MillisSinceEpoch,
                                   endMillis: MillisSinceEpoch,
                                   pointInTime: MillisSinceEpoch): Future[Either[CrunchStateError, Option[CrunchState]]] = {
    val query = CachableActorQuery(Props(classOf[CrunchStateReadActor], airportConfig.portStateSnapshotInterval, SDate(pointInTime), DrtStaticParameters.expireAfterMillis, airportConfig.queues), GetPortState(startMillis, endMillis))
    val portCrunchResult = cacheActorRef.ask(query)(new Timeout(30 seconds))
    portCrunchResult.map {
      case Some(PortState(f, m, s)) =>
        log.info(s"Got point-in-time PortState for ${SDate(pointInTime).toISOString()}")
        Right(Option(CrunchState(f.values.toSet, m.values.toSet, s.values.toSet)))
      case _ => Right(None)
    }.recover {
      case t =>
        log.warning(s"Didn't get a point-in-time CrunchState: $t")
        Left(CrunchStateError(t.getMessage))
    }
  }

  def exportDesksAndQueuesAtPointInTimeCSV(
                                            pointInTime: String,
                                            terminalName: TerminalName,
                                            startHour: Int,
                                            endHour: Int
                                          ): Action[AnyContent] = Action.async {

    log.info(s"Exports: For point in time ${SDate(pointInTime.toLong).toISOString()}")
    val portCode = airportConfig.portCode
    val pit = SDate(pointInTime.toLong)

    val fileName = f"$portCode-$terminalName-desks-and-queues-${pit.getFullYear()}-${pit.getMonth()}%02d-${pit.getDate()}%02dT" +
      f"${pit.getHours()}%02d-${pit.getMinutes()}%02d-hours-$startHour%02d-to-$endHour%02d"

    val crunchStateForPointInTime = loadBestCrunchStateForPointInTime(pit.millisSinceEpoch)
    exportDesksToCSV(terminalName, pit, startHour, endHour, crunchStateForPointInTime).map {
      case Some(csvData) =>
        val columnHeadings = CSVData.terminalCrunchMinutesToCsvDataHeadings(airportConfig.queues(terminalName))
        Result(
          ResponseHeader(200, Map("Content-Disposition" -> s"attachment; filename='$fileName.csv'")),
          HttpEntity.Strict(ByteString(columnHeadings + CSVData.lineEnding + csvData), Option("application/csv")))
      case None =>
        NotFound("Could not find desks and queues for this date.")
    }
  }

  def exportDesksToCSV(terminalName: TerminalName,
                       pointInTime: SDateLike,
                       startHour: Int,
                       endHour: Int,
                       crunchStateFuture: Future[Either[CrunchStateError, Option[CrunchState]]]
                      ): Future[Option[String]] = {

    val startDateTime = getLocalLastMidnight(pointInTime).addHours(startHour)
    val endDateTime = getLocalLastMidnight(pointInTime).addHours(endHour)
    val isInRange = isInRangeOnDay(startDateTime, endDateTime) _
    val localTime = SDate(pointInTime, europeLondonTimeZone)

    crunchStateFuture.map {
      case Right(Some(CrunchState(_, cm, sm))) =>
        log.debug(s"Exports: ${localTime.toISOString()} Got ${cm.size} CMs and ${sm.size} SMs ")
        val cmForDay: Set[CrunchMinute] = cm.filter(cm => isInRange(SDate(cm.minute, europeLondonTimeZone)))
        val smForDay: Set[StaffMinute] = sm.filter(sm => isInRange(SDate(sm.minute, europeLondonTimeZone)))
        log.debug(s"Exports: ${localTime.toISOString()} filtered to ${cmForDay.size} CMs and ${smForDay.size} SMs ")
        Option(CSVData.terminalCrunchMinutesToCsvData(cmForDay, smForDay, terminalName, airportConfig.queues(terminalName)))
      case unexpected =>
        log.error(s"Exports: Got the wrong thing $unexpected for Point In time: ${localTime.toISOString()}")

        None
    }
  }

  def exportForecastWeekToCSV(startDay: String, terminal: TerminalName): Action[AnyContent] = Action.async {
    val startOfWeekMidnight = getLocalLastMidnight(SDate(startDay.toLong))
    val endOfForecast = startOfWeekMidnight.addDays(180)
    val now = SDate.now()

    val startOfForecast = if (startOfWeekMidnight.millisSinceEpoch < now.millisSinceEpoch) {
      log.info(s"${startOfWeekMidnight.toLocalDateTimeString()} < ${now.toLocalDateTimeString()}, going to use ${getLocalNextMidnight(now)} instead")
      getLocalNextMidnight(now)
    } else startOfWeekMidnight

    val crunchStateFuture = ctrl.forecastCrunchStateActor.ask(
      GetPortState(startOfForecast.millisSinceEpoch, endOfForecast.millisSinceEpoch)
    )(new Timeout(30 seconds))

    val portCode = airportConfig.portCode

    val fileName = f"$portCode-$terminal-forecast-export-${startOfForecast.getFullYear()}-${startOfForecast.getMonth()}%02d-${startOfForecast.getDate()}%02d"
    crunchStateFuture.map {
      case Some(PortState(_, m, s)) =>
        log.info(s"Forecast CSV export for $terminal on $startDay with: crunch minutes: ${m.size} staff minutes: ${s.size}")
        val csvData = CSVData.forecastPeriodToCsv(ForecastPeriod(Forecast.rollUpForWeek(m.values.toSet, s.values.toSet, terminal)))
        Result(
          ResponseHeader(200, Map("Content-Disposition" -> s"attachment; filename='$fileName.csv'")),
          HttpEntity.Strict(ByteString(csvData), Option("application/csv"))
        )

      case None =>
        log.error(s"Forecast CSV Export: Missing planning data for ${startOfWeekMidnight.ddMMyyString} for Terminal $terminal")
        NotFound(s"Sorry, no planning summary available for week starting ${startOfWeekMidnight.ddMMyyString}")
    }
  }

  def exportForecastWeekHeadlinesToCSV(startDay: String, terminal: TerminalName): Action[AnyContent] = Action.async {
    val startOfWeekMidnight = getLocalLastMidnight(SDate(startDay.toLong))
    val endOfForecast = startOfWeekMidnight.addDays(180)
    val now = SDate.now()

    val startOfForecast = if (startOfWeekMidnight.millisSinceEpoch < now.millisSinceEpoch) {
      log.info(s"${startOfWeekMidnight.toLocalDateTimeString()} < ${now.toLocalDateTimeString()}, going to use ${getLocalNextMidnight(now)} instead")
      getLocalNextMidnight(now)
    } else startOfWeekMidnight

    val crunchStateFuture = ctrl.forecastCrunchStateActor.ask(
      GetPortState(startOfForecast.millisSinceEpoch, endOfForecast.millisSinceEpoch)
    )(new Timeout(30 seconds))


    val fileName = f"${airportConfig.portCode}-$terminal-forecast-export-headlines-${startOfForecast.getFullYear()}-${startOfForecast.getMonth()}%02d-${startOfForecast.getDate()}%02d"
    crunchStateFuture.map {
      case Some(PortState(_, m, _)) =>
        val csvData = CSVData.forecastHeadlineToCSV(Forecast.headLineFigures(m.values.toSet, terminal), airportConfig.exportQueueOrder)
        Result(
          ResponseHeader(200, Map("Content-Disposition" -> s"attachment; filename='$fileName.csv'")),
          HttpEntity.Strict(ByteString(csvData), Option("application/csv")
          )
        )

      case None =>
        log.error(s"Missing headline data for ${startOfWeekMidnight.ddMMyyString} for Terminal $terminal")
        NotFound(s"Sorry, no headlines available for week starting ${startOfWeekMidnight.ddMMyyString}")
    }
  }

  def exportApi(day: Int, month: Int, year: Int, terminalName: TerminalName) = authByRole(ApiViewPortCsv) {
    Action.async { request =>
      val dateOption = Try(SDate(year, month, day, 0, 0)).toOption
      val terminalNameOption = airportConfig.terminalNames.find(name => name == terminalName)
      val resultOption = for {
        date <- dateOption
        terminalName <- terminalNameOption
      } yield {
        val pit = date.millisSinceEpoch
        val crunchStateForPointInTime = loadBestCrunchStateForPointInTime(pit)
        val fileName = f"export-splits-$portCode-$terminalName-${date.getFullYear()}-${date.getMonth()}-${date.getDate()}"
        flightsForCSVExportWithinRange(terminalName, date, startHour = 0, endHour = 24, crunchStateForPointInTime).map {
          case Some(csvFlights) =>
            val csvData = CSVData.flightsWithSplitsWithAPIActualsToCSVWithHeadings(csvFlights)
            Result(
              ResponseHeader(OK, Map(
                CONTENT_LENGTH -> csvData.length.toString,
                CONTENT_TYPE -> "text/csv",
                CONTENT_DISPOSITION -> s"attachment; filename='$fileName.csv'",
                CACHE_CONTROL -> "no-cache")
              ),
              HttpEntity.Strict(ByteString(csvData), Option("application/csv"))
            )
          case None => NotFound("No data for this date")
        }
      }
      resultOption.getOrElse(
        Future(BadRequest("Invalid terminal name or date"))
      )
    }
  }

  def exportFlightsWithSplitsAtPointInTimeCSV(pointInTime: String,
                                              terminalName: TerminalName,
                                              startHour: Int,
                                              endHour: Int): Action[AnyContent] = Action.async {
    implicit request =>
      val pit = SDate(pointInTime.toLong)

      val portCode = airportConfig.portCode
      val fileName = f"$portCode-$terminalName-arrivals-${pit.getFullYear()}-${pit.getMonth()}%02d-${pit.getDate()}%02dT" +
        f"${pit.getHours()}%02d-${pit.getMinutes()}%02d-hours-$startHour%02d-to-$endHour%02d"

      val crunchStateForPointInTime = loadBestCrunchStateForPointInTime(pit.millisSinceEpoch)
      flightsForCSVExportWithinRange(terminalName, pit, startHour, endHour, crunchStateForPointInTime).map {
        case Some(csvFlights) =>
          val csvData = if (ctrl.getRoles(config, request.headers, request.session).contains(ApiView)) {
            log.info(s"Sending Flights CSV with ACL data to DRT Team member")
            CSVData.flightsWithSplitsWithAPIActualsToCSVWithHeadings(csvFlights)
          }
          else {
            log.info(s"Sending Flights CSV with no ACL data")
            CSVData.flightsWithSplitsToCSVWithHeadings(csvFlights)
          }
          Result(
            ResponseHeader(200, Map(
              "Content-Disposition" -> s"attachment; filename='$fileName.csv'",
              HeaderNames.CACHE_CONTROL -> "no-cache")
            ),
            HttpEntity.Strict(ByteString(csvData), Option("application/csv"))
          )
        case None => NotFound("No data for this date")
      }
  }

  def exportFlightsWithSplitsBetweenTimeStampsCSV(start: String,
                                                  end: String,
                                                  terminalName: TerminalName): Action[AnyContent] = Action.async {
    val startPit = getLocalLastMidnight(SDate(start.toLong, europeLondonTimeZone))
    val endPit = SDate(end.toLong, europeLondonTimeZone)

    val portCode = airportConfig.portCode
    val fileName = makeFileName("arrivals", terminalName, startPit, endPit, portCode)

    val dayRangeInMillis = startPit.millisSinceEpoch to endPit.millisSinceEpoch by oneDayMillis
    val days: Seq[Future[Option[String]]] = dayRangeInMillis.zipWithIndex.map {

      case (dayMillis, index) =>
        val csvFunc = if (index == 0) CSVData.flightsWithSplitsToCSVWithHeadings _ else CSVData.flightsWithSplitsToCSV _
        flightsForCSVExportWithinRange(
          terminalName = terminalName,
          pit = SDate(dayMillis),
          startHour = 0,
          endHour = 24,
          crunchStateFuture = loadBestCrunchStateForPointInTime(dayMillis)
        ).map {
          case Some(fs) => Option(csvFunc(fs))
          case None =>
            log.error(s"Missing a day of flights")
            None
        }
    }

    CSVData.multiDayToSingleExport(days).map(csvData => {
      Result(ResponseHeader(200, Map("Content-Disposition" -> s"attachment; filename='$fileName.csv'")),
        HttpEntity.Strict(ByteString(csvData), Option("application/csv")))
    })
  }

  def exportDesksAndQueuesBetweenTimeStampsCSV(start: String,
                                               end: String,
                                               terminalName: TerminalName): Action[AnyContent] = Action.async {
    val startPit = getLocalLastMidnight(SDate(start.toLong, europeLondonTimeZone))
    val endPit = SDate(end.toLong, europeLondonTimeZone)

    val portCode = airportConfig.portCode
    val fileName = makeFileName("desks-and-queues", terminalName, startPit, endPit, portCode)

    val dayRangeMillis = startPit.millisSinceEpoch to endPit.millisSinceEpoch by oneDayMillis
    val days: Seq[Future[Option[String]]] = dayRangeMillis.map(
      millis => exportDesksToCSV(
        terminalName = terminalName,
        pointInTime = SDate(millis),
        startHour = 0,
        endHour = 24,
        crunchStateFuture = loadBestCrunchStateForPointInTime(millis)
      )
    )

    CSVData.multiDayToSingleExport(days).map(csvData => {
      Result(ResponseHeader(200, Map("Content-Disposition" -> s"attachment; filename='$fileName.csv'")),
        HttpEntity.Strict(ByteString(
          CSVData.terminalCrunchMinutesToCsvDataHeadings(airportConfig.queues(terminalName)) + CSVData.lineEnding + csvData
        ), Option("application/csv")))
    })
  }

  def makeFileName(subject: String,
                   terminalName: TerminalName,
                   startPit: SDateLike,
                   endPit: SDateLike,
                   portCode: String): String = {
    f"$portCode-$terminalName-$subject-" +
      f"${startPit.getFullYear()}-${startPit.getMonth()}%02d-${startPit.getDate()}-to-" +
      f"${endPit.getFullYear()}-${endPit.getMonth()}%02d-${endPit.getDate()}"
  }

  def fetchAclFeed(portCode: String): Action[AnyContent] = Action.async {
    val sshClient = ctrl.aclFeed.ssh
    val sftpClient = ctrl.aclFeed.sftp(sshClient)
    val fileName = AclFeed.latestFileForPort(sftpClient, portCode.toUpperCase)

    log.info(s"Latest ACL file for $portCode: $fileName. Fetching..")

    val zipContent = AclFeed.contentFromFileName(sftpClient, fileName)
    val csvFileName = fileName.replace(".zip", ".csv")

    sshClient.disconnect()
    sftpClient.close()

    val result = Result(
      ResponseHeader(200, Map("Content-Disposition" -> s"attachment; filename='$csvFileName'")),
      HttpEntity.Strict(ByteString(zipContent), Option("application/csv"))
    )

    Future(result)
  }

  def isInRangeOnDay(startDateTime: SDateLike, endDateTime: SDateLike)(minute: SDateLike): Boolean =
    startDateTime.millisSinceEpoch <= minute.millisSinceEpoch && minute.millisSinceEpoch <= endDateTime.millisSinceEpoch


  def flightsForCSVExportWithinRange(
                                      terminalName: TerminalName,
                                      pit: SDateLike,
                                      startHour: Int,
                                      endHour: Int,
                                      crunchStateFuture: Future[Either[CrunchStateError, Option[CrunchState]]]
                                    ): Future[Option[List[ApiFlightWithSplits]]] = {

    val startDateTime = getLocalLastMidnight(pit).addHours(startHour)
    val endDateTime = getLocalLastMidnight(pit).addHours(endHour)
    val isInRange = isInRangeOnDay(startDateTime, endDateTime) _

    crunchStateFuture.map {
      case Right(Some(CrunchState(fs, _, _))) =>

        val flightsForTerminalInRange = fs.toList
          .filter(_.apiFlight.Terminal == terminalName)
          .filter(_.apiFlight.PcpTime.isDefined)
          .filter(f => isInRange(SDate(f.apiFlight.PcpTime.getOrElse(0L), europeLondonTimeZone)))

        Option(flightsForTerminalInRange)
      case unexpected =>
        log.error(s"got the wrong thing extracting flights from CrunchState (terminal: $terminalName, millis: $pit," +
          s" start hour: $startHour, endHour: $endHour): Error: $unexpected")
        None
    }
  }

  def saveStaff() = Action {
    implicit request =>
      val maybeShifts: Option[ShiftAssignments] = request.body.asJson.flatMap(ImportStaff.staffJsonToShifts)

      maybeShifts match {
        case Some(shifts) =>
          log.info(s"Received ${shifts.assignments.length} shifts. Sending to actor")
          ctrl.shiftsActor ! SetShifts(shifts.assignments)
          Created
        case _ =>
          BadRequest("{\"error\": \"Unable to parse data\"}")
      }
  }

  def authByRole[A](allowedRole: Role)(action: Action[A]) = Action.async(action.parser) { request =>
    val loggedInUser: LoggedInUser = ctrl.getLoggedInUser(config, request.headers, request.session)
    log.error(s"${loggedInUser.roles}, allowed role $allowedRole")
    if (loggedInUser.hasRole(allowedRole)) {
      auth(action)(request)
    } else {
      log.error("Unauthorized")
      Future(Unauthorized(s"{" +
        s"Permission denied, you need $allowedRole to access this page" +
        s"}"))
    }
  }

  def auth[A](action: Action[A]) = Action.async(action.parser) { request =>

    val loggedInUser: LoggedInUser = ctrl.getLoggedInUser(config, request.headers, request.session)
    val allowedRole = airportConfig.role

    val enablePortAccessRestrictions =
      config.getOptional[Boolean]("feature-flags.port-access-restrictions").getOrElse(false)

    val preventAccess = !loggedInUser.hasRole(allowedRole) && enablePortAccessRestrictions

    if (preventAccess) {
      Future(Unauthorized(s"{" +
        s"Permission denied, you need $allowedRole to access this port" +
        s"}"))
    } else {
      action(request)
    }
  }

  def autowireApi(path: String): Action[RawBuffer] = auth {
    Action.async(parse.raw) {
      implicit request =>
        log.info(s"Request path: $path")

        val b = request.body.asBytes(parse.UNLIMITED).get

        // call Autowire route

        implicit val staffAssignmentsPickler: CompositePickler[StaffAssignments] = compositePickler[StaffAssignments].addConcreteType[ShiftAssignments].addConcreteType[FixedPointAssignments]
        implicit val apiPaxTypeAndQueueCountPickler: Pickler[ApiPaxTypeAndQueueCount] = generatePickler[ApiPaxTypeAndQueueCount]
        implicit val feedStatusPickler: CompositePickler[FeedStatus] = compositePickler[FeedStatus].
          addConcreteType[FeedStatusSuccess].
          addConcreteType[FeedStatusFailure]

        val router = Router.route[Api](ApiService(airportConfig, ctrl.shiftsActor, ctrl.fixedPointsActor, ctrl.staffMovementsActor, request.headers, request.session))

        router(
          autowire.Core.Request(path.split("/"), Unpickle[Map[String, ByteBuffer]].fromBytes(b.asByteBuffer))
        ).map(buffer => {
          val data = Array.ofDim[Byte](buffer.remaining())
          buffer.get(data)
          Ok(data)
        })
    }
  }

  def logging: Action[Map[String, Seq[String]]] = auth {
    Action(parse.tolerantFormUrlEncoded) {
      implicit request =>

        def postStringValOrElse(key: String, default: String) = {
          request.body.get(key).map(_.head).getOrElse(default)
        }

        val logLevel = postStringValOrElse("level", "ERROR")

        val millis = request.body.get("timestamp")
          .map(_.head.toLong)
          .getOrElse(SDate.now(Crunch.europeLondonTimeZone).millisSinceEpoch)

        val logMessage = Map(
          "logger" -> ("CLIENT - " + postStringValOrElse("logger", "log")),
          "message" -> postStringValOrElse("message", "no log message"),
          "logTime" -> SDate(millis).toISOString(),
          "url" -> postStringValOrElse("url", request.headers.get("referrer").getOrElse("unknown url")),
          "logLevel" -> logLevel
        )

        log.log(Logging.levelFor(logLevel).getOrElse(Logging.ErrorLevel), s"Client Error: ${
          logMessage.map {
            case (value, key) => s"$key: $value"
          }.mkString(", ")
        }")

        Ok("logged successfully")
    }
  }
}

object Forecast {
  def headLineFigures(forecastMinutes: Set[CrunchMinute], terminalName: TerminalName): ForecastHeadlineFigures = {
    val headlines = forecastMinutes
      .toList
      .filter(_.terminalName == terminalName)
      .groupBy(
        cm => getLocalLastMidnight(SDate(cm.minute)).millisSinceEpoch
      )
      .flatMap {
        case (day, cm) =>
          cm.groupBy(_.queueName)
            .map {
              case (q, cms) =>
                QueueHeadline(
                  day,
                  q,
                  Math.round(cms.map(_.paxLoad).sum).toInt,
                  Math.round(cms.map(_.workLoad).sum).toInt
                )
            }
      }.toSet
    ForecastHeadlineFigures(headlines)
  }

  def rollUpForWeek(forecastMinutes: Set[CrunchMinute],
                    staffMinutes: Set[StaffMinute],
                    terminalName: TerminalName): Map[MillisSinceEpoch, Seq[ForecastTimeSlot]] = {
    val actualStaffByMinute = staffByTimeSlot(15)(staffMinutes, terminalName)
    val fixedPointsByMinute = fixedPointsByTimeSlot(15)(staffMinutes, terminalName)
    val terminalMinutes: Seq[(MillisSinceEpoch, Set[CrunchMinute])] = CrunchApi.terminalMinutesByMinute(forecastMinutes, terminalName)
    groupCrunchMinutesByX(15)(terminalMinutes, terminalName, Queues.queueOrder)
      .map {
        case (startMillis, cms) =>
          val available = actualStaffByMinute.getOrElse(startMillis, 0)
          val fixedPoints = fixedPointsByMinute.getOrElse(startMillis, 0)
          val forecastTimeSlot = ForecastTimeSlot(startMillis, available, required = fixedPoints)
          cms.foldLeft(forecastTimeSlot) {
            case (fts, cm) => fts.copy(required = fts.required + cm.deskRec)
          }
      }
      .groupBy(forecastTimeSlot => getLocalLastMidnight(SDate(forecastTimeSlot.startMillis)).millisSinceEpoch)
  }
}

case class GetTerminalCrunch(terminalName: TerminalName)
