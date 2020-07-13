package services.arrivals

import drt.shared.api.Arrival

case class ArrivalDataSanitiser(
                                 maybeEstimatedThresholdHours: Option[Int],
                                 maybeTaxiThresholdMinutes: Option[Int]) {

  def maybeEstThresholdMillis: Option[Long] = maybeEstimatedThresholdHours.map(h => h * 60 * 60 * 1000L)

  def maybeTaxiThresholdMillis: Option[Long] = maybeTaxiThresholdMinutes.map(m => m * 60 * 1000L)

  def withSaneEstimates(arrival: Arrival): Arrival = withSaneEstimatedChox(withSaneEstimatedTouchDown(arrival))

  def withSaneEstimatedTouchDown(arrival: Arrival): Arrival =
    (maybeEstThresholdMillis, arrival.sstimated) match {
      case (Some(threshold), Some(est)) if (Math.abs(est - arrival.scheduled) > threshold) =>
        arrival.copy(sstimated = None)
      case _ => arrival
    }

  def withSaneEstimatedChox(arrival: Arrival): Arrival =
    (maybeTaxiThresholdMillis, maybeEstThresholdMillis, arrival.estimatedChox, arrival.sstimated) match {
      case (_, Some(threshold), Some(est), _) if (Math.abs(est - arrival.scheduled) > threshold) =>
        arrival.copy(estimatedChox = None)
      case (_, _, Some(estChox), Some(est)) if estChox <= est =>
        arrival.copy(estimatedChox = None)
      case (Some(taxiThreshold), _, Some(estChox), Some(est)) if est + taxiThreshold < estChox =>
        arrival.copy(estimatedChox = None)
      case _ => arrival
    }
}

object ArrivalDataSanitiser {
  val arrivalDataSanitiserWithoutThresholds = ArrivalDataSanitiser(None, None)
}
