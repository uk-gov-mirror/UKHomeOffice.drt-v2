package drt.chroma

import akka.NotUsed
import akka.actor.Cancellable
import akka.stream.scaladsl.Source
import drt.chroma.chromafetcher.ChromaFetcher
import drt.chroma.chromafetcher.ChromaFetcher.{ChromaFlightLike, ChromaForecastFlight, ChromaLiveFlight}
import drt.server.feeds.Implicits._
import drt.shared.FlightsApi.Flights
import drt.shared.Terminals.Terminal
import drt.shared.api.Arrival
import drt.shared.{ForecastFeedSource, LiveFeedSource, Operator}
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.util.StringUtils
import server.feeds.{ArrivalsFeedFailure, ArrivalsFeedResponse, ArrivalsFeedSuccess}
import services.SDate

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

object StreamingChromaFlow {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def chromaPollingSource[X <: ChromaFlightLike](chromaFetcher: ChromaFetcher[X],
                                                 pollFrequency: FiniteDuration,
                                                 toDrtArrival: Seq[X] => List[Arrival])(implicit ec: ExecutionContext): Source[ArrivalsFeedResponse, Cancellable] = {
    Source.tick(1 milliseconds, pollFrequency, NotUsed)
      .mapAsync(1) { _ =>
        chromaFetcher.currentFlights
          .map {
            case Success(flights) => ArrivalsFeedSuccess(Flights(toDrtArrival(flights)))
            case Failure(t) => ArrivalsFeedFailure(t.getMessage)
          }
          .recoverWith { case t =>
            Future(ArrivalsFeedFailure(t.getMessage))
          }
      }
  }

  def liveChromaToArrival(chromaArrivals: Seq[ChromaLiveFlight]): List[Arrival] = {
    chromaArrivals.map(flight => {
      val walkTimeMinutes = 4
      val pcpTime: Long = org.joda.time.DateTime.parse(flight.SchDT).plusMinutes(walkTimeMinutes).getMillis
      val est = Try(SDate(flight.EstDT).millisSinceEpoch).getOrElse(0L)
      val act = Try(SDate(flight.ActDT).millisSinceEpoch).getOrElse(0L)
      val estChox = Try(SDate(flight.EstChoxDT).millisSinceEpoch).getOrElse(0L)
      val actChox = Try(SDate(flight.ActChoxDT).millisSinceEpoch).getOrElse(0L)
      Arrival(
        operator = if (flight.Operator.isEmpty) None else Option(Operator(flight.Operator)),
        status = flight.Status,
        estimated = if (est == 0) None else Option(est),
        actual = if (act == 0) None else Option(act),
        estimatedChox = if (estChox == 0) None else Option(estChox),
        actualChox = if (actChox == 0) None else Option(actChox),
        gate = if (StringUtils.isEmpty(flight.Gate)) None else Option(flight.Gate),
        stand = if (StringUtils.isEmpty(flight.Stand)) None else Option(flight.Stand),
        maxPax = if (flight.MaxPax == 0) None else Option(flight.MaxPax),
        actPax = if (flight.ActPax == 0) None else Option(flight.ActPax),
        tranPax = if (flight.ActPax == 0) None else Option(flight.TranPax),
        runwayID = if (StringUtils.isEmpty(flight.RunwayID)) None else Option(flight.RunwayID),
        baggageReclaimId = if (StringUtils.isEmpty(flight.BaggageReclaimId)) None else Option(flight.BaggageReclaimId),
        airportID = flight.AirportID,
        terminal = Terminal(flight.Terminal),
        rawICAO = flight.ICAO,
        rawIATA = flight.IATA,
        origin = flight.Origin,
        pcpTime = Some(pcpTime),
        scheduled = SDate(flight.SchDT).millisSinceEpoch,
        feedSources = Set(LiveFeedSource)
      )
    }).toList
  }

  def forecastChromaToArrival(chromaArrivals: Seq[ChromaForecastFlight]): List[Arrival] = {
    chromaArrivals.map(flight => {
      val walkTimeMinutes = 4
      val pcpTime: Long = org.joda.time.DateTime.parse(flight.SchDT).plusMinutes(walkTimeMinutes).getMillis
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
        actPax = if (flight.EstPax == 0) None else Option(flight.EstPax),
        tranPax = if (flight.EstPax == 0) None else Option(flight.EstTranPax),
        runwayID = None,
        baggageReclaimId = None,
        airportID = flight.AirportID,
        terminal = Terminal(flight.Terminal),
        rawICAO = flight.ICAO,
        rawIATA = flight.IATA,
        origin = flight.Origin,
        pcpTime = Option(pcpTime),
        feedSources = Set(ForecastFeedSource),
        scheduled = SDate(flight.SchDT).millisSinceEpoch
      )
    }).toList
  }

}
