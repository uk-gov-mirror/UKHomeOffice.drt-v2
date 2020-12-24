package manifests.passengers

import drt.shared.SplitRatiosNs.{SplitSource, SplitSources}
import drt.shared.{SDateLike, _}
import manifests.UniqueArrivalKey
import passengersplits.core.PassengerTypeCalculatorValues.{CountryCodes, DocumentType}
import passengersplits.parsing.VoyageManifestParser.{PassengerInfoJson, VoyageManifest}
import services.SDate

case class BestAvailableManifest(source: SplitSource,
                                 arrivalPortCode: PortCode,
                                 departurePortCode: PortCode,
                                 voyageNumber: VoyageNumberLike,
                                 carrierCode: CarrierCode,
                                 scheduled: SDateLike,
                                 passengerList: List[ManifestPassengerProfile]) {

  def maybeArrivalKey: Option[ArrivalKey] = voyageNumber match {
    case vn: VoyageNumber => Option(ArrivalKey(departurePortCode, vn, scheduled.millisSinceEpoch))
    case _ => None
  }
}

case class BestAvailableManifests(manifests: List[BestAvailableManifest]) {
  def toMap: Map[ArrivalKey, BestAvailableManifest] = manifests
    .map(bm => bm.maybeArrivalKey -> bm)
    .collect {
      case (Some(key), bm) => key -> bm
    }
    .toMap
}

object BestAvailableManifest {
  def apply(manifest: VoyageManifest): BestAvailableManifest = {

    val uniquePax: List[PassengerInfoJson] = if (manifest.PassengerList.exists(_.PassengerIdentifier.exists(_ != "")))
      manifest.PassengerList.collect {
        case p@PassengerInfoJson(_, _, _, _, _, _, _, _, Some(id)) if id != "" => p
      }
        .map { passengerInfo =>
          passengerInfo.PassengerIdentifier -> passengerInfo
        }
        .toMap
        .values
        .toList
    else
      manifest.PassengerList

    BestAvailableManifest(
      SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
      manifest.ArrivalPortCode,
      manifest.DeparturePortCode,
      manifest.VoyageNumber,
      manifest.CarrierCode,
      manifest.scheduleArrivalDateTime.getOrElse(SDate.now()),
      uniquePax.map(p => ManifestPassengerProfile(p, manifest.ArrivalPortCode))
    )
  }

  def apply(source: SplitSource,
            uniqueArrivalKey: UniqueArrivalKey,
            passengerList: List[ManifestPassengerProfile]): BestAvailableManifest = BestAvailableManifest(
    source,
    uniqueArrivalKey.arrivalPort,
    uniqueArrivalKey.departurePort,
    uniqueArrivalKey.voyageNumber,
    CarrierCode(""),
    uniqueArrivalKey.scheduled,
    passengerList)
}

case class ManifestPassengerProfile(nationality: Nationality,
                                    documentType: Option[DocumentType],
                                    age: Option[PaxAge],
                                    inTransit: Option[Boolean])

object ManifestPassengerProfile {
  def apply(pij: PassengerInfoJson, portCode: PortCode): ManifestPassengerProfile = {
    val nationality = pij.NationalityCountryCode.getOrElse(Nationality(""))
    val documentType: Option[DocumentType] = if (nationality.code == CountryCodes.UK)
      Option(DocumentType.Passport)
    else
      pij.DocumentType
    val maybeInTransit = Option(pij.InTransitFlag.isInTransit || pij.DisembarkationPortCode.exists(_ != portCode))
    ManifestPassengerProfile(nationality, documentType, pij.Age, maybeInTransit)
  }
}
