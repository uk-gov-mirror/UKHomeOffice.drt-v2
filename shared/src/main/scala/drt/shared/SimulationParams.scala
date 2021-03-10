package drt.shared

import drt.shared.CrunchApi.{CrunchMinutes, DeskRecMinutes}
import drt.shared.FlightsApi.FlightsWithSplits
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared.dates.LocalDate
import upickle.default.{ReadWriter, macroRW}

import scala.util.Try

case class SimulationParams(
                             terminal: Terminal,
                             date: LocalDate,
                             passengerWeighting: Double,
                             processingTimes: Map[PaxTypeAndQueue, Int],
                             minDesks: Map[Queue, Int],
                             maxDesks: Map[Queue, Int],
                             eGateBanksSize: Int,
                             slaByQueue: Map[Queue, Int],
                             crunchOffsetMinutes: Int
                           ) {
  def applyToAirportConfig(airportConfig: AirportConfig) = {
    val openDesks: Map[Queues.Queue, (List[Int], List[Int])] = airportConfig.minMaxDesksByTerminalQueue24Hrs(terminal).map {
      case (q, (origMinDesks, origMaxDesks)) =>

        val newMaxDesks = origMaxDesks.map(d => maxDesks.getOrElse(q, d))
        val newMinDesks = origMinDesks.map(d => minDesks.getOrElse(q, d))
        q -> (newMinDesks, newMaxDesks)
    }

    airportConfig.copy(
      minMaxDesksByTerminalQueue24Hrs = airportConfig.minMaxDesksByTerminalQueue24Hrs + (terminal -> openDesks),
      eGateBankSize = eGateBanksSize,
      slaByQueue = airportConfig.slaByQueue.map {
        case (q, v) => q -> slaByQueue.getOrElse(q, v)
      },
      crunchOffsetMinutes = crunchOffsetMinutes,
      terminalProcessingTimes = airportConfig.terminalProcessingTimes + (terminal -> airportConfig.terminalProcessingTimes(terminal).map {
        case (ptq, defaultValue) =>
          ptq -> processingTimes.get(ptq)
            .map(s => s.toDouble / 60)
            .getOrElse(defaultValue)
      }),
    )

  }

  def applyPassengerWeighting(flightsWithSplits: FlightsWithSplits) =
    FlightsWithSplits(flightsWithSplits.flights.map {
      case (ua, fws) => ua -> fws.copy(
        apiFlight = fws
          .apiFlight.copy(
          ActPax = fws.apiFlight.ActPax.map(n => (n * passengerWeighting).toInt),
          TranPax = fws.apiFlight.TranPax.map(n => (n * passengerWeighting).toInt)
        ))
    })

  def toQueryStringParams: String = {
    List(
      s"terminal=$terminal",
      s"date=$date",
      s"passengerWeighting=$passengerWeighting",
      s"eGateBankSize=$eGateBanksSize",
      s"crunchOffsetMinutes=$crunchOffsetMinutes"
    ) ::
      processingTimes.map {
        case (ptq, value) => s"${ptq.key}=$value"
      } ::
      minDesks.map {
        case (q, value) => s"${q}_min=$value"
      } ::
      maxDesks.map {
        case (q, value) => s"${q}_max=$value"
      } ::
      slaByQueue.map {
        case (q, value) => s"${q}_sla=$value"
      } :: Nil
  }.flatten.mkString("&")

}

case class SimulationResult(params: SimulationParams, queueToCrunchMinutes: Map[Queues.Queue, List[CrunchApi.CrunchMinute]])

object SimulationResult {
  implicit val rw: ReadWriter[SimulationResult] = macroRW
}

object SimulationParams {

  implicit val rw: ReadWriter[SimulationParams] = macroRW

  def apply(terminal: Terminal, date: LocalDate, airportConfig: AirportConfig): SimulationParams = SimulationParams(
    terminal,
    date,
    1.0,
    airportConfig.terminalProcessingTimes(terminal).mapValues(m => (m * 60).toInt),
    airportConfig.minMaxDesksByTerminalQueue24Hrs(terminal).map {
      case (q, (min, _)) => q -> min.max
    },
    airportConfig.minMaxDesksByTerminalQueue24Hrs(terminal).map {
      case (q, (_, max)) => q -> max.max
    },
    eGateBanksSize = airportConfig.eGateBankSize,
    slaByQueue = airportConfig.slaByQueue,
    crunchOffsetMinutes = 0
  )

  val requiredFields = List(
    "terminal",
    "date",
    "passengerWeighting",
    "eGateBankSize",
    "crunchOffsetMinutes"
  )

  def fromQueryStringParams(qsMap: Map[String, Seq[String]]): Try[SimulationParams] = Try {

    val maybeSimulationFieldsStrings: Map[String, Option[String]] = requiredFields
      .map(f => f -> qsMap.get(f).flatMap(_.headOption)).toMap

    val qMinDesks = queueParams(qsMap, "_min")
    val qMaxDesks = queueParams(qsMap, "_max")
    val qSlas = queueParams(qsMap, "_sla")

    val procTimes: Map[PaxTypeAndQueue, Int] = PaxTypesAndQueues.inOrder.map(ptq => {
      ptq -> qsMap.get(ptq.key).flatMap(v => v.headOption.flatMap(s => Try(s.toInt).toOption))
    }).collect {
      case (k, Some(v)) => k -> v
    }.toMap

    val maybeParams = for {
      terminal: String <- maybeSimulationFieldsStrings("terminal")
      dateString: String <- maybeSimulationFieldsStrings("date")
      localDate <- LocalDate.parse(dateString)
      passengerWeightingString: String <- maybeSimulationFieldsStrings("passengerWeighting")

      eGateBankSizeString: String <- maybeSimulationFieldsStrings("eGateBankSize")
      crunchOffsetMinutes: String <- maybeSimulationFieldsStrings("crunchOffsetMinutes")
    } yield SimulationParams(
      Terminal(terminal),
      localDate,
      passengerWeightingString.toDouble,
      procTimes,
      qMinDesks,
      qMaxDesks,
      eGateBankSizeString.toInt,
      qSlas,
      crunchOffsetMinutes.toInt
    )

    maybeParams match {
      case Some(simulationParams) => simulationParams
      case None =>
        throw new Exception(s"Missing ${
          maybeSimulationFieldsStrings.collect {
            case (k, None) => k
          }.mkString(", ")
        }")
    }

  }

  def queueParams(qsMap: Map[String, Seq[String]], suffix: String): Map[Queue, Int] = qsMap
    .collect {
      case (k, value) if k.contains(suffix) =>
        Queue(k.replace(suffix, "")) -> value.headOption.flatMap(s => Try(s.toInt).toOption)
    }
    .collect {
      case (k, Some(v)) => k -> v
    }

}
