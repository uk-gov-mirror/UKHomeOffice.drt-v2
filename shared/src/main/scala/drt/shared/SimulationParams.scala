package drt.shared

import drt.shared.FlightsApi.FlightsWithSplits
import drt.shared.Queues.Queue
import drt.shared.Terminals.Terminal
import drt.shared.dates.LocalDate
import upickle.default.{ReadWriter, macroRW}

import scala.util.{Success, Try}

case class SimulationParams(
                             terminal: Terminal,
                             date: LocalDate,
                             passengerWeighting: Double,
                             processingTimes: Map[PaxTypeAndQueue, Int],
                             minDesks: Map[Queue, Int],
                             maxDesks: Map[Queue, Int],
                             eGateBanksSizes: IndexedSeq[Int],
                             slaByQueue: Map[Queue, Int],
                             crunchOffsetMinutes: Int,
                             eGateOpenHours: Seq[Int],
                           ) {

  def applyToAirportConfig(airportConfig: AirportConfig): AirportConfig = {
    val openDesks: Map[Queues.Queue, (List[Int], List[Int])] = airportConfig.minMaxDesksByTerminalQueue24Hrs(terminal).map {
      case (q, (origMinDesks, origMaxDesks)) =>

        val newMaxDesks = origMaxDesks.map(d => maxDesks.getOrElse(q, d))
        val newMinDesks = origMinDesks.map(d => minDesks.getOrElse(q, d))
        q -> (newMinDesks, newMaxDesks)
    }

    airportConfig.copy(
      minMaxDesksByTerminalQueue24Hrs = airportConfig.minMaxDesksByTerminalQueue24Hrs + (terminal -> openDesks),
      eGateBankSizes = Map(terminal -> eGateBanksSizes),
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
      queueStatusProvider = QueueStatusProviders.FlexibleEGatesForSimulation(eGateOpenHours)
    )

  }

  def applyPassengerWeighting(flightsWithSplits: FlightsWithSplits): FlightsWithSplits =
    FlightsWithSplits(flightsWithSplits.flights.map {
      case (ua, fws) => ua -> fws.copy(
        apiFlight = fws
          .apiFlight.copy(
          ActPax = fws.apiFlight.ActPax.map(n => (n * passengerWeighting).toInt),
          TranPax = fws.apiFlight.TranPax.map(n => (n * passengerWeighting).toInt)
        ))
    })
}

case class SimulationResult(params: SimulationParams, queueToCrunchMinutes: Map[Queues.Queue, List[CrunchApi.CrunchMinute]])

object SimulationResult {
  implicit val rw: ReadWriter[SimulationResult] = macroRW
}

object SimulationParams {

  implicit val rw: ReadWriter[SimulationParams] = macroRW

  val fullDay = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)

  val requiredFields = List(
    "terminal",
    "date",
    "passengerWeighting",
    "eGateBankSizes",
    "crunchOffsetMinutes",
    "eGateOpenHours"
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
      terminalName: String <- maybeSimulationFieldsStrings("terminal")
      dateString: String <- maybeSimulationFieldsStrings("date")
      localDate <- LocalDate.parse(dateString)
      passengerWeightingString: String <- maybeSimulationFieldsStrings("passengerWeighting")
      eGateBankSizeString: String <- maybeSimulationFieldsStrings("eGateBankSizes")
      crunchOffsetMinutes: String <- maybeSimulationFieldsStrings("crunchOffsetMinutes")
      eGateOpenHours: String <- maybeSimulationFieldsStrings("eGateOpenHours")
    } yield {
      val terminal = Terminal(terminalName)
      SimulationParams(
        terminal,
        localDate,
        passengerWeightingString.toDouble,
        procTimes,
        qMinDesks,
        qMaxDesks,
        eGateBankSizeString.split(",").map(_.toInt),
        qSlas,
        crunchOffsetMinutes.toInt,
        eGateOpenHours.split(",").map(s => Try(s.toInt)).collect {
          case Success(i) => i
        }
      )
    }

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
