package drt.client.services.handlers

import diode.data.{Pending, Pot, Ready}
import diode.{ActionResult, Effect, ModelRW}
import drt.client.actions.Actions._
import drt.client.services.{DrtApi, PollDelay}
import drt.shared.{AirportInfo, PortCode}
import upickle.default.read

import scala.collection.immutable.Map
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

class AirportCountryHandler[M](modelRW: ModelRW[M, Map[PortCode, Pot[AirportInfo]]]) extends LoggingActionHandler(modelRW) {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case GetAirportInfos(codes) =>
      effectOnly(Effect(DrtApi.get(s"airport-info?portCode=${codes.map(_.iata).mkString(",")}")
        .map { response =>
          val codeToInfo = read[Map[PortCode, AirportInfo]](response.responseText)
          UpdateAirportInfos(codeToInfo)
        }
        .recoverWith {
          case _ => Future(RetryActionAfter(GetAirportConfig, PollDelay.recoveryDelay))
        }))
    case UpdateAirportInfos(infos) =>
      val infosReady = infos.map(kv => (kv._1, Ready(kv._2)))
      updated(value ++ infosReady)
    case UpdateAirportInfo(code, Some(airportInfo)) =>
      val newValue = value + (code -> Ready(airportInfo))
      updated(newValue)
  }
}
