package drt.server.feeds.lhr

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import drt.server.feeds.Implicits._
import drt.server.feeds.lhr.forecast.LHRForecastFlightRow
import drt.shared.FlightsApi.Flights
import drt.shared.{ForecastFeedSource, api}
import drt.shared.Terminals.Terminal
import drt.shared.api.Arrival
import org.slf4j.{Logger, LoggerFactory}
import server.feeds.{ArrivalsFeedFailure, ArrivalsFeedResponse, ArrivalsFeedSuccess, GetFeedImportArrivals}
import services.SDate

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.Try

case class LHRForecastFeed(arrivalsActor: ActorRef)(implicit timeout: Timeout) {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def requestFeed: Future[ArrivalsFeedResponse] =
    arrivalsActor.ask(GetFeedImportArrivals)
      .map {
        case Some(Flights(arrivals)) =>
          log.info(s"Got ${arrivals.length} LHR port forecast arrivals")
          ArrivalsFeedSuccess(Flights(arrivals), SDate.now())
        case x =>
          log.info(s"Got no LHR port forecast arrivals: $x")
          ArrivalsFeedSuccess(Flights(Seq()), SDate.now())
      }
      .recoverWith {
        case e => Future(ArrivalsFeedFailure(e.getMessage, SDate.now()))
      }
}

object LHRForecastFeed {
  def log: Logger = LoggerFactory.getLogger(getClass)

  def lhrFieldsToArrival(flightRow: LHRForecastFlightRow): Try[Arrival] = {
    Try {
      Arrival(
        operator = None,
        status = "Port Forecast",
        estimated = None,
        actual = None,
        estimatedChox = None,
        actualChox = None,
        gate = None,
        stand = None,
        maxPax = None,
        actPax = if (flightRow.totalPax == 0) None else Option(flightRow.totalPax),
        tranPax = if (flightRow.totalPax == 0) None else Option(flightRow.transferPax),
        runwayID = None,
        baggageReclaimId = None,
        airportID = "LHR",
        terminal = Terminal(flightRow.terminal),
        rawICAO = flightRow.flightCode.replace(" ", ""),
        rawIATA = flightRow.flightCode.replace(" ", ""),
        origin = flightRow.origin,
        scheduled = flightRow.scheduledDate.millisSinceEpoch,
        pcpTime = None,
        feedSources = Set(ForecastFeedSource)
      )
    }
  }
}
