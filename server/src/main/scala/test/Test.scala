package test.controllers

import javax.inject.{Inject, Singleton}

import akka.actor.{ActorRef, ActorSystem}
import akka.stream.Materializer
import akka.util.Timeout
import controllers.AirportConfProvider
import drt.chroma.chromafetcher.ChromaFetcher.ChromaLiveFlight
import drt.chroma.chromafetcher.ChromaParserProtocol._
import drt.shared.{Arrival, LiveFeedSource, SDateLike}
import org.slf4j.{Logger, LoggerFactory}
import passengersplits.parsing.VoyageManifestParser.FlightPassengerInfoProtocol._
import passengersplits.parsing.VoyageManifestParser.{VoyageManifest, VoyageManifests}
import play.api.http.HeaderNames
import play.api.mvc.{InjectedController, Session}
import play.api.{Configuration, Environment}
import services.SDate
import spray.json._
import test.ResetData
import test.TestActors.ResetActor
import test.roles.MockRoles
import test.roles.MockRoles.MockRolesProtocol._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

@Singleton
class Test @Inject()(implicit val config: Configuration,
                     implicit val mat: Materializer,
                     env: Environment,
                     val system: ActorSystem,
                     ec: ExecutionContext) extends InjectedController with AirportConfProvider {
  implicit val timeout: Timeout = Timeout(250 milliseconds)

  val log: Logger = LoggerFactory.getLogger(getClass)

  val baseTime: SDateLike = SDate.now()

  val liveArrivalsTestActor: Future[ActorRef] = system.actorSelection(s"akka://${portCode.toLowerCase}-drt-actor-system/user/TestActor-LiveArrivals").resolveOne()
  val apiManifestsTestActor: Future[ActorRef] = system.actorSelection(s"akka://${portCode.toLowerCase}-drt-actor-system/user/TestActor-APIManifests").resolveOne()
  val staffMovementsTestActor: Future[ActorRef] = system.actorSelection(s"akka://${portCode.toLowerCase}-drt-actor-system/user/TestActor-StaffMovements").resolveOne()
  val mockRolesTestActor: Future[ActorRef] = system.actorSelection(s"akka://${portCode.toLowerCase}-drt-actor-system/user/TestActor-MockRoles").resolveOne()

  def saveArrival(arrival: Arrival) = {
    liveArrivalsTestActor.map(actor => {

      actor ! arrival
    })
  }

  def saveVoyageManifest(voyageManifest: VoyageManifest) = {
    apiManifestsTestActor.map(actor => {

      log.info(s"Sending Splits: $voyageManifest to Test Actor")

      actor ! VoyageManifests(Set(voyageManifest))
    })
  }

  def resetData() = {
    system.actorSelection(s"akka://${portCode.toLowerCase}-drt-actor-system/user/TestActor-ResetData").resolveOne().map(actor => {

      log.info(s"Sending reset message")

      actor ! ResetData
    })

    liveArrivalsTestActor.map(_ ! ResetActor)
    apiManifestsTestActor.map(_ ! ResetActor)
    staffMovementsTestActor.map(_ ! ResetActor)
  }

  def addArrival() = Action {
    implicit request =>

      request.body.asJson.map(s => s.toString.parseJson.convertTo[ChromaLiveFlight]) match {
        case Some(flight) =>
          val walkTimeMinutes = 4
          val pcpTime: Long = org.joda.time.DateTime.parse(flight.SchDT).plusMinutes(walkTimeMinutes).getMillis
          val actPax = Some(flight.ActPax).filter(_ != 0)
          val arrival = Arrival(
            Operator = if (flight.Operator.contains("")) None else Some(flight.Operator),
            Status = flight.Status,
            Estimated = Some(SDate(flight.EstDT).millisSinceEpoch),
            Actual = Some(SDate(flight.ActDT).millisSinceEpoch),
            EstimatedChox = Some(SDate(flight.EstChoxDT).millisSinceEpoch),
            ActualChox = Some(SDate(flight.ActChoxDT).millisSinceEpoch),
            Gate = Some(flight.Gate),
            Stand = Some(flight.Stand),
            MaxPax = Some(flight.MaxPax).filter(_ != 0),
            ActPax = actPax,
            TranPax = if (actPax.isEmpty) None else Some(flight.TranPax),
            RunwayID = Some(flight.RunwayID),
            BaggageReclaimId = Some(flight.BaggageReclaimId),
            FlightID = if (flight.FlightID == 0) None else Some(flight.FlightID),
            AirportID = flight.AirportID,
            Terminal = flight.Terminal,
            rawICAO = flight.ICAO,
            rawIATA = flight.IATA,
            Origin = flight.Origin,
            PcpTime = Some(pcpTime),
            FeedSources = Set(LiveFeedSource),
            Scheduled = SDate(flight.SchDT).millisSinceEpoch
          )
          saveArrival(arrival)
          Created
        case None =>
          BadRequest(s"Unable to parse JSON: ${request.body.asText}")
      }
  }

  object ArrivalsCSVFixture {

    object fieldMap {
      val Estimated = 4
      val Actual = 5
      val EstimatedChox = 6
      val ActualChox = 7
      val Stand = 8
      val MaxPax = 9
      val ActPax = 10
      val TranPax = 11
      val Terminal = 0
      val rawICAO = 1
      val rawIATA = 1
      val Origin = 2
      val Scheduled = 3
    }

  }

  def addArrivals(forDate: String) = Action {
    implicit request =>

      def timeToSDateOnDate(time: String) = SDate.tryParseString(forDate + "T" + time + "Z")
        .toOption
        .map(_.millisSinceEpoch)

      request.body.asMultipartFormData.flatMap(_.files.find(_.key == "data")) match {
        case Some(f) =>
          log.info(s"Got this file: ${f.ref.toString}")
          csvPathToRows(f.ref.path.toString).drop(1).foreach(csvRow => {
            val fields = csvRow.split(",")
            import ArrivalsCSVFixture.fieldMap._
            val arrivalTry = Try(Arrival(
              None,
              "Unk",
              timeToSDateOnDate(fields(Estimated)),
              timeToSDateOnDate(fields(Actual)),
              timeToSDateOnDate(fields(EstimatedChox)),
              timeToSDateOnDate(fields(ActualChox)),
              None,
              Option(fields(Stand)),
              Option(fields(MaxPax).toInt),
              Option(fields(ActPax).toInt),
              Option(fields(TranPax).toInt),
              None,
              None,
              None,
              "TEST",
              fields(Terminal),
              fields(rawICAO),
              fields(rawIATA),
              fields(Origin),
              timeToSDateOnDate(fields(Scheduled)).getOrElse(SDate.now().millisSinceEpoch),
              None,
              Set(LiveFeedSource),
              None
            )) match {
              case Success(a) => saveArrival(a)
              case Failure(f) => log.error("Failed to parse Arrival $f")
            }
          })
          Created.withHeaders(HeaderNames.ACCEPT -> "application/csv")

        case None =>
          BadRequest("You must post a CSV file with name \"data\"")
      }

  }

  def csvPathToRows(fileName: String): Iterator[String] = {
    val bufferedSource = scala.io.Source.fromFile(fileName)
    bufferedSource.getLines()
  }

  def addManifest() = Action {
    implicit request =>

      request.body.asJson.map(s => s.toString.parseJson.convertTo[VoyageManifest]) match {
        case Some(vm) =>
          log.info(s"Got a manifest to save $vm")
          saveVoyageManifest(vm)
          Created
        case None =>
          BadRequest(s"Unable to parse JSON: ${request.body.asText}")
      }
  }

  def saveMockRoles(roles: MockRoles) = mockRolesTestActor.map(a => a ! roles)

  def setMockRoles() = Action {
    implicit request =>
      request.body.asJson.map(s => s.toString.parseJson.convertTo[MockRoles]) match {
        case Some(roles) =>
          log.info(s"Got mock roles to set: $roles")

          log.info(s"Replacing these mock roles: ${request.session.data}")
          log.info(s"mock headers: ${request.headers}")

          Created.withSession(Session(Map("mock-roles" -> roles.roles.map(_.name).mkString(","))))
        case None =>
          BadRequest(s"Unable to parse JSON: ${request.body.asText}")
      }
  }

  def deleteAllData() = Action {
    implicit request =>

      resetData()

      Accepted
  }
}
