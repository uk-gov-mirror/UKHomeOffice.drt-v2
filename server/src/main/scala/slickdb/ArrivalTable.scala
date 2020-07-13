package slickdb

import java.sql.Timestamp

import drt.shared
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.PortCode
import drt.shared.Terminals.Terminal
import drt.shared.api.Arrival
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps


case class AggregatedArrivals(arrivals: Seq[AggregatedArrival])

case class AggregatedArrival(code: String, scheduled: MillisSinceEpoch, origin: String, destination: String, terminalName: String)

object AggregatedArrival {
  def apply(arrival: Arrival, destination: String): AggregatedArrival = AggregatedArrival(
    arrival.flightCode,
    arrival.scheduled,
    arrival.origin.toString,
    destination,
    terminalName = arrival.terminal.toString
  )
}

case class ArrivalTable(portCode: PortCode, tables: Tables) extends ArrivalTableLike {
  val log: Logger = LoggerFactory.getLogger(getClass)

  import tables.profile.api._
  import tables.{Arrival, ArrivalRow}

  val db: tables.profile.backend.DatabaseDef = Database.forConfig("aggregated-db")
  val arrivalsTableQuery = TableQuery[Arrival]

  def selectAll: AggregatedArrivals = {
    val eventualArrivals = db.run(arrivalsTableQuery.result).map(arrivalRows =>
      arrivalRows.map(ar =>
        AggregatedArrival(ar.code, ar.scheduled.getTime, ar.origin, ar.destination, ar.terminal)))
    val arrivals = Await.result(eventualArrivals, 5 seconds)
    AggregatedArrivals(arrivals)
  }

  def removeArrival(number: Int, terminal: Terminal, scheduledTs: Timestamp): Future[Int] = {
    val idx = matchIndex(number, terminal, scheduledTs)
    log.info(s"removing: $number / ${terminal.toString} / $scheduledTs")
    db.run(arrivalsTableQuery.filter(idx).delete) recover {
      case throwable =>
        log.error(s"delete failed", throwable)
        0
    }
  }

  def insertOrUpdateArrival(f: shared.api.Arrival): Future[Int] = {
    db.run(arrivalsTableQuery.insertOrUpdate(arrivalRow(f))) recover {
      case throwable =>
        log.error(s"insertOrUpdate failed", throwable)
        0
    }
  }

  def matchIndex(number: Int, terminal: Terminal, scheduledTs: Timestamp): tables.Arrival => Rep[Boolean] = (arrival: Arrival) =>
    arrival.number === number &&
      arrival.terminal === terminal.toString &&
      arrival.scheduled === scheduledTs &&
      arrival.destination === portCode.toString

  def arrivalRow(f: shared.api.Arrival): tables.ArrivalRow = {
    val sch = new Timestamp(f.scheduled)
    val est = f.sstimated.map(new Timestamp(_))
    val act = f.actual.map(new Timestamp(_))
    val estChox = f.estimatedChox.map(new Timestamp(_))
    val actChox = f.actualChox.map(new Timestamp(_))
    val pcp = new Timestamp(f.pcpTime.getOrElse(f.scheduled))
    val pcpPax = f.actPax.map(ap => ap - f.tranPax.getOrElse(0))

    ArrivalRow(f.flightCode, f.voyageNumber.numeric, portCode.iata, f.origin.toString, f.terminal.toString, f.gate, f.stand, f.status.description, sch, est, act, estChox, actChox, pcp, f.actPax, pcpPax)
  }
}
