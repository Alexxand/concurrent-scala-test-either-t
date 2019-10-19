package example

import java.time.LocalDateTime
import java.util.concurrent.Executors

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext
import cats.data._
import cats.implicits._

object Main extends App {
  private implicit val blockingEc =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  def run(ids: List[Int]): EitherT[Future, FuckingError, BlackResult] = {
    for {
      red <- EitherT.right(ids.map(redRunOrDefault(_)).sequence)
      green <- greenRunOrError(red)(blockingEc)
      blue <- EitherT.right(blueRun(green))
      black <- blackRunOrError(blue)
    } yield black
  }

  val run1 = run(1.to(10).toList)
  val run2 = run(1.to(10).toList)
  val run3 = run(1.to(10).toList)

  (for {
    _ <- EitherT.right[FuckingError](Future{println(LocalDateTime.now())})
    _ <- run1
    _ <- run2
    _ <- run3
    _ <- EitherT.right[FuckingError](Future{println(LocalDateTime.now())})
  } yield ()).leftMap(
    e =>
      println(e)
  )

  private def redRunOrDefault(id: Int)(implicit ec: ExecutionContext):Future[RedResult] =
    Future{
      Try(RedAdapter.run(id)).getOrElse(RedAdapter.defaultResult(id))
    }

  private def greenRunOrError(red: List[RedResult])(ec: ExecutionContext): EitherT[Future,FuckingError,GreenResult] =
    EitherT(
      GreenAdapter.run(red)(ec).transform {
        case Success(result) => Success(Right(result))
        case Failure(e) => Success(Left(GreenFuckingError(e.getMessage)))
      }(ec)
    )

  private def blueRun(green: GreenResult): Future[BlueResult] = {
    val promise = Promise[BlueResult]()
    BlueAdapter.run(green, promise.success)
    promise.future
  }

  private def blackRunOrError(blue: BlueResult)(implicit ec: ExecutionContext): EitherT[Future,FuckingError,BlackResult] = {
    val blackResource = BlackAdapter.init()
    EitherT(
      Future {
        val blackResultOrError =
          Try(blackResource.run(blue)).fold[Either[BlackFuckingError, BlackResult]](
            e => Left(BlackFuckingError(e.getMessage)),
            Right(_)
          )
        blackResource.destroy()
        blackResultOrError
      }
    )
  }
}

trait FuckingError
case class GreenFuckingError(message: String) extends FuckingError
case class BlackFuckingError(message: String) extends FuckingError