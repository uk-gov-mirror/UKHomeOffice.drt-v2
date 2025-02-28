package actors.minutes

import actors.minutes.MinutesActorLike.{MinutesLookup, MinutesUpdate}
import actors.routing.RouterActorLikeWithSubscriber
import akka.actor.ActorRef
import drt.shared.CrunchApi.{CrunchMinute, DeskRecMinute, MinutesContainer}
import drt.shared.TQM
import drt.shared.Terminals.Terminal
import drt.shared.dates.UtcDate

import scala.language.postfixOps

class QueueMinutesActor(terminals: Iterable[Terminal],
                        lookup: MinutesLookup[CrunchMinute, TQM],
                        updateMinutes: MinutesUpdate[CrunchMinute, TQM],
                        val updatesSubscriber: ActorRef)
  extends MinutesActorLike(terminals, lookup, updateMinutes) with RouterActorLikeWithSubscriber[MinutesContainer[CrunchMinute, TQM], (Terminal, UtcDate)] {

  override def shouldSendEffectsToSubscriber: MinutesContainer[CrunchMinute, TQM] => Boolean = _.contains(classOf[DeskRecMinute])
}
