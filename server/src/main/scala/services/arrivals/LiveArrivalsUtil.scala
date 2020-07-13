package services.arrivals

import drt.shared.api.Arrival
import services.SDate

object LiveArrivalsUtil {

  def mergePortFeedWithBase(portFeedArrival: Arrival, baseLiveArrival: Arrival): Arrival = {

    portFeedArrival.copy(
      actualChox = if (portFeedArrival.actualChox.isEmpty) baseLiveArrival.actualChox else portFeedArrival.actualChox,
      actual = if (portFeedArrival.actual.isEmpty) baseLiveArrival.actual else portFeedArrival.actual,
      estimatedChox = if (portFeedArrival.estimatedChox.isEmpty) baseLiveArrival.estimatedChox else portFeedArrival.estimatedChox,
      sstimated = if (portFeedArrival.sstimated.isEmpty) baseLiveArrival.sstimated else portFeedArrival.sstimated,
      gate = if (portFeedArrival.gate.isEmpty) baseLiveArrival.gate else portFeedArrival.gate,
      status = if (portFeedArrival.status.description == "UNK") baseLiveArrival.status else portFeedArrival.status
    )
  }

  def printArrival(a: Arrival): String = {
    s"""
       |flightCode: ${a.flightCode}
       |terminal: ${a.terminal}
       |scheduled: ${SDate(a.scheduled).toISOString()}
       |Est: ${a.sstimated.map(d => SDate(d).toISOString())}
       |EstChox: ${a.estimatedChox.map(d => SDate(d).toISOString())}
       |Act: ${a.actual.map(d => SDate(d).toISOString())}
       |ActChox: ${a.actualChox.map(d => SDate(d).toISOString())}
       |Status: ${a.status.description}
       |Gate: ${a.gate}
       |PCP: ${a.pcpTime.map(d => SDate(d).toISOString())}
       |""".stripMargin
  }

}

