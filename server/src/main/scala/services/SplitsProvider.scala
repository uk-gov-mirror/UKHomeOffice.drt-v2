package services

import com.typesafe.config.{Config, ConfigFactory}
import drt.shared.SplitRatiosNs.SplitRatios
import drt.shared.api.Arrival
import drt.shared.MilliDate
import org.slf4j.{Logger, LoggerFactory}

object SplitsProvider {
  type SplitProvider = (String, MilliDate) => Option[SplitRatios]
  val log: Logger = LoggerFactory.getLogger(getClass)

  def splitsForFlight(providers: List[SplitProvider])(apiFlight: Arrival): Option[SplitRatios] = {
    providers.foldLeft(None: Option[SplitRatios])((prev, provider) => {
      prev match {
        case Some(_) => prev
        case None => provider(apiFlight.flightCode, MilliDate(apiFlight.scheduled))
      }
    })
  }

  def shouldUseCsvSplitsProvider: Boolean = {
    val config: Config = ConfigFactory.load
    log.info(s"splitsProvider: csv path ${config.getString("passenger_splits_csv_url")}")
    config.hasPath("passenger_splits_csv_url") && config.getString("passenger_splits_csv_url") != ""
  }

  def emptyProvider: SplitProvider = (_, _) => Option.empty[SplitRatios]

  def csvProvider: SplitProvider = {
    if (shouldUseCsvSplitsProvider) {
      log.info("SplitsProvider: Using csv splits provider")
      val provider: (String, MilliDate) => Option[SplitRatios] = CSVPassengerSplitsProvider(CsvPassengerSplitsReader.flightPaxSplitsLinesFromConfig).splitRatioProvider
      provider
    }
    else {
      log.info("SplitsProvider: using emptyProvider")
      emptyProvider
    }
  }
}
