package drt.shared

import drt.shared.api.Arrival

object CodeShares {
  def uniqueArrivalsWithCodeShares[GenFlight](apiFlightFromGenFlight: (GenFlight) => Arrival)
                                             (flights: Seq[GenFlight]): List[(GenFlight, Set[Arrival])] = {
    val grouped = flights.groupBy(f =>
      (apiFlightFromGenFlight(f).scheduled, apiFlightFromGenFlight(f).terminal, apiFlightFromGenFlight(f).origin)
    )
    grouped.values.map(flights => {
      val mainFlight: GenFlight = flights.sortBy(f => apiFlightFromGenFlight(f).actPax).reverse.head
      val shares: Set[Arrival] = flights
        .filter(_ != mainFlight)
        .toSet
        .map(apiFlightFromGenFlight)

      (mainFlight, shares)
    }).toList
  }
}
