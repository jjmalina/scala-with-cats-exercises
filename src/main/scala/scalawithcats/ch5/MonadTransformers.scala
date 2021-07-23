package scalawithcats.ch5

import cats.data.EitherT
import cats.instances.future._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object MonadsTransformAndRollOut {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(level) => EitherT.right(Future.successful(level))
      case None => EitherT.left(Future.successful(s"${autobot} is unreachable"))
    }

    def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
      for {
        powerLevel1 <- getPowerLevel(ally1)
        powerlevel2 <- getPowerLevel(ally2)
      } yield powerLevel1 + powerlevel2 > 15

    def tacticalReport(ally1: String, ally2: String): String = {
      val result = Await.result(canSpecialMove(ally1, ally2).value, 1.second)
      result match {
        case Right(true) => s"${ally1} and ${ally2} are ready to roll out!"
        case Right(false) => s"${ally1} and ${ally2} need a recharge."
        case Left(err) => f"Comms error: ${err}"
      }
    }
}
