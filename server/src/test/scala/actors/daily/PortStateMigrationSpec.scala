package actors.daily

import actors.{DbStreamingJournal, InMemoryStreamingJournal, ProdUpdatesActorProps}
import actors.acking.AckingReceiver.{Ack, StreamCompleted, StreamInitialized}
import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.persistence.query.{EventEnvelope, PersistenceQuery}
import akka.stream.{ActorMaterializer, KillSwitches, UniqueKillSwitch}
import akka.stream.scaladsl.{Keep, Sink}
import akka.testkit.TestKit
import com.typesafe.config.ConfigFactory
import drt.shared.{AirportConfig, AirportConfigs, PortCode, SDateLike}
import org.specs2.mutable.SpecificationLike
import services.SDate
import services.crunch.CrunchTestLike

import scala.collection.immutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._



class PortStateMigrationSpec extends TestKit(ActorSystem("jdbc-system", ConfigFactory.load("application-acp.conf"))) with SpecificationLike {
  implicit val mat: ActorMaterializer = ActorMaterializer.create(system)
  val journalType: DbStreamingJournal.type = DbStreamingJournal
  val persistenceId: String = "crunch-state"
  val seqNr1 = 1
  object Now {
    var now: SDateLike = SDate("2017-01-01")
    def setTo(newNow: SDateLike): Unit = now = newNow
  }
  val fluidNow: () => SDateLike = () => Now.now
  val airportConfig: AirportConfig = AirportConfigs.confByPort(PortCode(ConfigFactory.load().getString("portcode").toUpperCase))
  val queueUpdatesSupervisor: ActorRef = system.actorOf(Props(new UpdatesSupervisor(fluidNow, airportConfig.terminals.toList, ProdUpdatesActorProps.queues(fluidNow, journalType))))
//  val staffUpdatesSupervisor: ActorRef = system.actorOf(Props(new UpdatesSupervisor(fluidNow, airportConfig.terminals.toList, ProdUpdatesActorProps.staff(fluidNow, journalType))))

  "Given a persistence id" >> {
    "When I ask for the events from the beginning of time (seq nr 1)" >> {
      "Then I can see the CrunchDiffMessage messages" >> {
        val (ks, eventualEvents): (UniqueKillSwitch, Future[immutable.Seq[EventEnvelope]]) = PersistenceQuery(system)
          .readJournalFor[journalType.ReadJournalType](journalType.id)
          .eventsByPersistenceId(persistenceId, seqNr1, 3)
          .viaMat(KillSwitches.single)(Keep.right)
          .toMat(Sink.seq)(Keep.both)
          .run()

        val result: immutable.Seq[EventEnvelope] = Await.result(eventualEvents, 5 seconds)

        println(s"Got ${result.length} events")
        success
      }
    }
  }

  "Given source of crunch state events" >> {
    "When I ask for them to be migrated" >> {
      "I can load the the events from the new actors" >> {
        success
      }
    }
  }
}
