package services.crunch

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.Materializer
import org.slf4j.{Logger, LoggerFactory}
import services.{OptimizerConfig, TryRenjin}
import upickle.default.{macroRW, ReadWriter => RW}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

trait OptimiserLike {
  def requestDesksAndWaits(workloadToOptimise: WorkloadToOptimise): Future[Option[DesksAndWaits]]
}

case class Optimiser(uri: String)(implicit val system: ActorSystem, val materializer: Materializer, val ec: ExecutionContext) extends OptimiserLike {
  val log: Logger = LoggerFactory.getLogger(getClass)

  import upickle.default.{write, read => rd}

  def requestDesksAndWaits(workloadToOptimise: WorkloadToOptimise): Future[Option[DesksAndWaits]] = {
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = Uri(uri),
      entity = HttpEntity(ContentTypes.`application/json`, write(workloadToOptimise))
    )
    val responseFuture = Http()
      .singleRequest(request)
      .map {
        case HttpResponse(StatusCodes.Created, _, entity, _) =>
          val responseBody = Await.result(entity.toStrict(120 seconds), 120 seconds).data.utf8String
          Try(rd[DesksAndWaits](responseBody)).toOption
        case HttpResponse(status, _, _, _) =>
          log.error(s"Got status: $status")
          None
      }
      .recoverWith {
        case throwable =>
          log.error("Caught error while retrieving optimised desks.", throwable)
          Future(None)
      }
    responseFuture
  }
}

final case class DesksAndWaits(desks: Seq[Int], waits: Seq[Int])

object DesksAndWaits {
  implicit val rw: RW[DesksAndWaits] = macroRW
}

final case class WorkloadToOptimise(workloads: Seq[Double], minDesks: Seq[Int], maxDesks: Seq[Int], sla: Int, description: String)

object WorkloadToOptimise {
  implicit val rw: RW[WorkloadToOptimise] = macroRW
}

object OptimiserLocal extends OptimiserLike {
  import scala.concurrent.ExecutionContext.Implicits.global

  override def requestDesksAndWaits(workloadToOptimise: WorkloadToOptimise): Future[Option[DesksAndWaits]] =
    TryRenjin.crunch(
      workloadToOptimise.workloads,
      workloadToOptimise.minDesks,
      workloadToOptimise.maxDesks,
      OptimizerConfig(workloadToOptimise.sla)
    ) match {
      case Success(result) => Future(Option(DesksAndWaits(result.recommendedDesks, result.waitTimes)))
      case Failure(_) => Future(None)
    }
}
