package actors

import drt.shared.SplitRatiosNs.SplitSources
import drt.shared._
import manifests.passengers.{BestAvailableManifest, BestAvailableManifests, ManifestPassengerProfile}
import org.specs2.mutable.Specification
import passengersplits.core.PassengerTypeCalculatorValues.DocumentType
import services.SDate

class BestAvailableManifestsMessageConversionSpec extends Specification {

  "Given a BestAvailableManifests if I serialise it and de-serialise it it should be the same" >> {

    val originalManifests = BestAvailableManifests(
      List(BestAvailableManifest(
        source = SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages,
        arrivalPortCode = PortCode("LHR"),
        departurePortCode = PortCode("TST"),
        voyageNumber = VoyageNumber(100),
        carrierCode = CarrierCode("TST"),
        scheduled = SDate("2020-12-24T11:00:00Z"),
        passengerList = List(
          ManifestPassengerProfile(
            nationality = Nationality("GBR"),
            documentType = Option(DocumentType("P")),
            age = Option(PaxAge(33)),
            inTransit = Option(false)
          )
        )
      ))
    )

    val serialised = BestAvailableManifestMessageConversion.manifestsToMessage(originalManifests)

    val result = BestAvailableManifestMessageConversion.manifestsFromMessage(serialised)

    result === originalManifests
  }

}
