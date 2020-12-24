package actors

import drt.shared.SplitRatiosNs.SplitSource
import drt.shared._
import manifests.passengers.{BestAvailableManifest, BestAvailableManifests, ManifestPassengerProfile}
import passengersplits.core.PassengerTypeCalculatorValues.DocumentType
import server.protobuf.messages.BestAvailableManifest.{BestAvailableManifestMessage, BestAvailableManifestsMessage, ManifestPassengerProfileMessage}
import services.SDate

object BestAvailableManifestMessageConversion {
  def manifestsFromMessage(message: BestAvailableManifestsMessage): BestAvailableManifests = BestAvailableManifests(
    message.manifests.map(manifestFromMessage).toList)

  def manifestFromMessage(message: BestAvailableManifestMessage) = {
    BestAvailableManifest(
      source = SplitSource(message.splitSource.getOrElse("")),
      arrivalPortCode = PortCode(message.arrivalPortCode.getOrElse("Missing Port Code")),
      departurePortCode = PortCode(message.departurePortCode.getOrElse("Missing Port Code")),
      voyageNumber = VoyageNumber(s"${message.voyageNumber.getOrElse("Inavlid")}"),
      carrierCode = CarrierCode(message.carrierCode.getOrElse("Missing Carrier Code")),
      scheduled = SDate(message.scheduled.getOrElse(0L)),
      passengerList = message.passengerList.map(manifestPassengerProfileFromMessage).toList
    )
  }

  def manifestPassengerProfileFromMessage(message: ManifestPassengerProfileMessage) = ManifestPassengerProfile(
    nationality = Nationality(message.nationality.getOrElse("missing nationality")),
    documentType = message.documentType.map(DocumentType(_)),
    age = message.age.map(PaxAge(_)),
    inTransit = message.inTransit
  )

  def manifestsToMessage(bam: BestAvailableManifests): BestAvailableManifestsMessage = BestAvailableManifestsMessage(
    Option(System.currentTimeMillis()),
    bam
      .manifests
      .map(bm => BestAvailableManifestMessage(
        arrivalPortCode = Option(bm.arrivalPortCode.iata),
        departurePortCode = Option(bm.departurePortCode.iata),
        voyageNumber = Option(bm.voyageNumber.numeric),
        carrierCode = Option(bm.carrierCode.code),
        scheduled = Option(bm.scheduled.millisSinceEpoch),
        passengerList = bm.passengerList.map(manifestPassengerProfileToMessage)
      ))
  )

  def manifestPassengerProfileToMessage(mpp: ManifestPassengerProfile) = ManifestPassengerProfileMessage(
    documentType = mpp.documentType.map(_.toString),
    age = mpp.age.map(_.years),
    inTransit = mpp.inTransit
  )


}
