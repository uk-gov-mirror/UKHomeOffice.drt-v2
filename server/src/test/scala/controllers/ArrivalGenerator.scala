package controllers

import drt.shared.Terminals.{T1, Terminal}

import drt.shared.api.Arrival
import drt.shared.{ArrivalStatus, FeedSource, Operator, PortCode}
import org.springframework.util.StringUtils
import services.SDate

object ArrivalGenerator {
  def arrival(iata: String = "",
              icao: String = "",
              schDt: String = "",
              actPax: Option[Int] = None,
              maxPax: Option[Int] = None,
              terminal: Terminal = T1,
              origin: PortCode = PortCode(""),
              operator: Option[Operator] = None,
              status: ArrivalStatus = ArrivalStatus(""),
              estDt: String = "",
              actDt: String = "",
              estChoxDt: String = "",
              actChoxDt: String = "",
              pcpDt: String = "",
              gate: Option[String] = None,
              stand: Option[String] = None,
              tranPax: Option[Int] = None,
              runwayId: Option[String] = None,
              baggageReclaimId: Option[String] = None,
              airportId: PortCode = PortCode(""),
              feedSources: Set[FeedSource] = Set(),
              apiPax: Option[Int] = None
             ): Arrival = {
    val pcpTime = if (pcpDt.nonEmpty) Option(SDate(pcpDt).millisSinceEpoch) else if (schDt.nonEmpty) Option(SDate(schDt).millisSinceEpoch) else None

    Arrival(
      rawICAO = icao,
      rawIATA = iata,
      actPax = actPax,
      terminal = terminal,
      origin = origin,
      operator = operator,
      status = status,
      estimated = if (!StringUtils.isEmpty(estDt)) Option(SDate.parseString(estDt).millisSinceEpoch) else None,
      actual = if (!StringUtils.isEmpty(actDt)) Option(SDate.parseString(actDt).millisSinceEpoch) else None,
      estimatedChox = if (!StringUtils.isEmpty(estChoxDt)) Option(SDate.parseString(estChoxDt).millisSinceEpoch) else None,
      actualChox = if (!StringUtils.isEmpty(actChoxDt)) Option(SDate.parseString(actChoxDt).millisSinceEpoch) else None,
      gate = gate,
      stand = stand,
      maxPax = maxPax,
      tranPax = tranPax,
      runwayID = runwayId,
      baggageReclaimId = baggageReclaimId,
      airportID = airportId,
      pcpTime = pcpTime,
      scheduled = if (!StringUtils.isEmpty(schDt)) SDate(schDt).millisSinceEpoch else 0,
      feedSources = feedSources,
      apiPax = apiPax
    )
  }
}
