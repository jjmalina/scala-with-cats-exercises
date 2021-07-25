package scalawithcats.ch7

import cats.Applicative
import cats.data.Validated
import cats.syntax.apply._ // for mapN
import cats.syntax.applicative._ // for pure
import cats.instances.vector._ // for Applicative
import cats.instances.option._ // for Applicative
import cats.instances.list._ // for Monoid

object TraverseUsage {
  def listTraverse[F[_]: Applicative, A, B]
      (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (accum, item) => (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

  type ErrorsOr[A] = Validated[List[String], A]

  def processValidated(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if(n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }
}

object TraverseApp extends App {
  import TraverseUsage._

  // 7.2.2.1 Exercise: Traversing with Vectors
  // becomes Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
  println(listSequence(List(Vector(1, 2), Vector(3, 4))))

  // 7.2.2.2 Exercise: Traversing with Options
  // will return Some(List(2, 4, 6))
  println(process(List(2, 4, 6)))
  // will return None
  println(process(List(1, 2, 3)))

  // 7.2.2.3 Exercise: Traversing with Validated
  // will return Valid(List(2, 4, 6))
  println(processValidated(List(2, 4, 6)))
  // will return Invalid(List("1 is not even", "3 is not even"))
  println(processValidated(List(1, 2, 3)))

}
