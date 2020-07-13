package drt.client.components

import drt.client.services.JSDateConversions.SDate
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.Terminals.Terminal
import drt.shared.api.Arrival
import drt.shared.{ApiFeedSource, ArrivalStatus, Operator, PortCode}


object ArrivalGenerator {

  def apiFlight(
                 iata: String = "",
                 icao: String = "",
                 schDt: String = "",
                 actPax: Option[Int] = None,
                 maxPax: Option[Int] = None,
                 terminal: Terminal = Terminal("T1"),
                 origin: PortCode = PortCode(""),
                 operator: Option[Operator] = None,
                 status: ArrivalStatus = ArrivalStatus(""),
                 estDt: String = "",
                 actDt: String = "",
                 estChoxDt: String = "",
                 actChoxDt: String = "",
                 gate: Option[String] = None,
                 stand: Option[String] = None,
                 tranPax: Option[Int] = None,
                 runwayId: Option[String] = None,
                 baggageReclaimId: Option[String] = None,
                 airportId: PortCode = PortCode(""),
                 pcpTime: Option[MillisSinceEpoch] = None
               ): Arrival =
    Arrival(
      operator = operator,
      status = status,
      estimated = if (estDt != "") Some(SDate(estDt).millisSinceEpoch) else None,
      actual = if (actDt != "") Some(SDate(actDt).millisSinceEpoch) else None,
      estimatedChox = if (estChoxDt != "") Some(SDate(estChoxDt).millisSinceEpoch) else None,
      actualChox = if (actChoxDt != "") Some(SDate(actChoxDt).millisSinceEpoch) else None,
      gate = gate,
      stand = stand,
      maxPax = maxPax,
      actPax = actPax,
      tranPax = tranPax,
      runwayID = runwayId,
      baggageReclaimId = baggageReclaimId,
      airportID = airportId,
      terminal = terminal,
      rawICAO = icao,
      rawIATA = iata,
      origin = origin,
      pcpTime = if (pcpTime.isDefined) Option(pcpTime.get) else if (schDt != "") Some(SDate(schDt).millisSinceEpoch) else None,
      scheduled = if (schDt != "") SDate(schDt).millisSinceEpoch else 0L,
      feedSources = Set(ApiFeedSource)
    )
}
