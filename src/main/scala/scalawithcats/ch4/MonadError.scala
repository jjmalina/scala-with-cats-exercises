package scalawithcats.ch4

import scala.util.Try
import cats.MonadError
import cats.instances.try_._ // for MonadError
import cats.instances.either._ // for MonadError
import cats.syntax.monadError._

object Validator {
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    me.pure(age).ensure(
      new IllegalArgumentException("Age must be greater than or requal to 18")
    )(_ >= 18)
}

object MonadErrorApp extends App {
  import Validator._

  println(validateAdult[Try](18))
  println(validateAdult[Try](8))
  type ExceptionOr[A] = Either[Throwable, A]
  println(validateAdult[ExceptionOr](-1))
}
