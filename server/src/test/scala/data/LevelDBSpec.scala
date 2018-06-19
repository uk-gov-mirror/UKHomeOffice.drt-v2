package data

import java.sql.{Connection, PreparedStatement}

import actors.serializers.ProtoBufSerializer
import akka.actor.ActorSystem
import akka.persistence.query.PersistenceQuery
import akka.persistence.query.journal.leveldb.scaladsl.LeveldbReadJournal
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import javax.sql.DataSource
import org.apache.commons.dbcp.BasicDataSource
import org.specs2.matcher.Scope
import org.specs2.mutable.Specification
import play.api.libs.json.Json

import scala.concurrent.duration._
import scala.concurrent.Await

class LevelDBSpec extends Specification {

  trait Context extends Scope with UsingPostgres with UsingDatabase

  "LevelDB" should {

    "can read the data" in new Context {
      implicit val system = ActorSystem()
      implicit val mat = ActorMaterializer()

      val columnNames = List( "persistence_id", "sequence_number", "deleted", "tags", "message")


      val protoBufSerializer = new ProtoBufSerializer

      val readJournal = PersistenceQuery(system).readJournalFor[LeveldbReadJournal](
        LeveldbReadJournal.Identifier)

      val result = readJournal.allPersistenceIds().runForeach{persistenceId =>
        println("\n\n persistenceId\n\n")
        println(persistenceId)
        println("===================\n\n")
        val res = readJournal.currentEventsByPersistenceId(persistenceId).map{ event =>
          val data = protoBufSerializer.toBinary(event.event.asInstanceOf[AnyRef])
          List( event.persistenceId, event.sequenceNr, false, null, data)
        }.runWith(Sink.seq)

        val scheduleData = Await.result(res, 300.seconds)

        withDatasource { implicit dataSource =>
          dataToDatabase("journal", columnNames, scheduleData.toIterator)
        }
      }
      Await.result(result, 300.seconds)
      ok("!")
    }

  }

}

// From: http://phil-rice.github.io/scala/performance/2015/10/30/Inserting-data-to-database-tables-with-scala.html
case class DataSourceDefn(url: String, userName: String, password: String,
                          classDriveName: String = "org.postgresql.Driver",
                          maxConnections: Integer = -1)

trait UsingPostgres {
  implicit lazy val defn: DataSourceDefn =
    DataSourceDefn(url = "jdbc:postgresql:ema",
      userName = "ema", password = "ema")
}

trait UsingDatabase {
  implicit def defn: DataSourceDefn

  protected def createDataSource(implicit dsDefn: DataSourceDefn) = {
    val ds = new BasicDataSource()
    ds.setDriverClassName(dsDefn.classDriveName)
    ds.setUrl(dsDefn.url)
    ds.setUsername(dsDefn.userName)
    ds.setPassword(dsDefn.password)
    ds.setMaxActive(dsDefn.maxConnections)
    ds
  }

  protected def dataToDatabase(tableName: String, columnNames: List[String], data: Iterator[List[Any]])(implicit ds: DataSource) = {
    val columnsWithCommas = columnNames.mkString(",")
    val questionMarks = columnNames.map(_ => "?").mkString(",")
    val sql = s"insert into $tableName ($columnsWithCommas) values ($questionMarks)"
    for ((list, lineNo) <- data.zipWithIndex)
      withPreparedStatement(sql, { implicit statement =>
        for ((value, index) <- list.zipWithIndex)
          statement.setObject(index + 1, value)
        statement.execute
      })
  }

  protected def withDatasource[X](fn: (DataSource) => X)(implicit defn: DataSourceDefn) = {
    val ds = createDataSource
    try fn(ds) finally ds.close
  }


  protected def withConnection[X](fn: (Connection => X))(implicit ds: DataSource) = {
    val c = ds.getConnection
    try fn(c) finally c.close
  }

  protected def withPreparedStatement[X](sql: String, fn: (PreparedStatement) => X)(
    implicit ds: DataSource) = withConnection { connection =>
    val statement = connection.prepareStatement(sql)
    try fn(statement) finally statement.close
  }
}