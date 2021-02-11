package slickdb

import org.slf4j.{Logger, LoggerFactory}


case class VoyageManifestPassengerInfoTable(tables: Tables, configName: String) {
  val log: Logger = LoggerFactory.getLogger(getClass)

  import tables.profile.api._
  import tables.VoyageManifestPassengerInfo

  val db: tables.profile.backend.DatabaseDef = Database.forConfig(configName)
  val voyageManifestPassengerInfoTableQuery = TableQuery[VoyageManifestPassengerInfo]
}
