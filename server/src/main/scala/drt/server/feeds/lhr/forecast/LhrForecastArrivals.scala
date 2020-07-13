package drt.server.feeds.lhr.forecast

import drt.server.feeds.Implicits._
import drt.shared.Terminals.Terminal
import drt.shared.api.Arrival
import drt.shared.{ForecastFeedSource, SDateLike}
import org.slf4j.{Logger, LoggerFactory}
import services.SDate
import services.graphstages.Crunch.europeLondonTimeZone

import scala.util.{Failure, Success, Try}

object LhrForecastArrivals {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def apply(lines: Seq[String]): Seq[Arrival] = {
    lines
      .map(line => LhrForecastArrival(line))
      .filter {
        case Success(_) => true
        case Failure(t) =>
          log.info(s"couldn't parse: $t")
          false
      }
      .collect {
        case Success(a) => a
      }
  }
}

object LhrForecastArrival {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def terminal(fields: Seq[String]): Terminal = Terminal(s"T${fields(0)}")

  def isArrival(fields: Seq[String]): Boolean = fields(1) == "A"

  def scheduledStr(fields: Seq[String]): String = {
    val date = fields(3)
    val time = fields(4)
    s"${date}T$time"
  }

  def scheduled(fields: Seq[String]): SDateLike = {
    SDate(scheduledStr(fields), europeLondonTimeZone)
  }

  def carrierCode(fields: Seq[String]): String = fields(5)

  def flightCode(fields: Seq[String]): String = fields(6).replace(" ", "")

  def origin(fields: Seq[String]): String = fields(7)

  def maxPax(fields: Seq[String]): Int = fields(12).toInt

  def paxTotal(fields: Seq[String]): Int = fields(13).toInt

  def paxTransit(fields: Seq[String]): Int = fields(14).toInt

  def apply(line: String): Try[Arrival] = {
    val fields = line.split(",")

    Try {
      val operator = carrierCode(fields)
      val maxPaxField = maxPax(fields)
      val actPax = paxTotal(fields)
      val transPax = paxTransit(fields)
      Arrival(
        operator = operator,
        status = "Forecast",
        estimated = None,
        actual = None,
        estimatedChox = None,
        actualChox = None,
        gate = None,
        stand = None,
        maxPax = Option(maxPaxField),
        actPax = Option(actPax),
        tranPax = Option(transPax),
        runwayID = None,
        baggageReclaimId = None,
        airportID = "LHR",
        terminal = terminal(fields),
        rawIATA = flightCode(fields),
        rawICAO = flightCode(fields),
        origin = origin(fields),
        scheduled = scheduled(fields).millisSinceEpoch,
        pcpTime = None,
        feedSources = Set(ForecastFeedSource)
      )
    } match {
      case Failure(t) =>
        log.info(s"Couldn't parse $line")
        Failure(t)
      case s => s
    }
  }
}
