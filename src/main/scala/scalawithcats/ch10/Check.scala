package scalawithcats.ch10

import cats.Semigroup
import cats.syntax.either._
// for asLeft and asRight
import cats.syntax.semigroup._ // for |+|
import cats.instances.list._ // for Semigroup
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.apply._

object CheckStuff {
  /* first non ADT implementation */
  case class CheckF[E,A](f: A => Either[E,A]) {
    def apply(value: A): Either[E, A] = f(value)

    /* 10.3 Semigroup, |+| */
    /* 10.4
    We should not short circuit because providing all errors to the user will
    make the most sense to them.
    */
    def and(that: CheckF[E,A])(implicit s: Semigroup[E]): CheckF[E,A] = CheckF { value =>
      val x = f(value)
      val y = that.apply(value)
      (x, y) match {
        case (Left(e), Left(e2)) => Left(e |+| e2)
        case (Right(_), Left(e)) => e.asLeft
        case (Left(e), Right(_)) => e.asLeft
        case (Right(a), Right(_)) => a.asRight  // which val?
      }
    }
  }

  sealed trait Check[E, A] {
    import Check._

    def and(that: Check[E,A]): Check[E,A] = And(this, that)
    def or(that: Check[E,A]): Check[E,A] = Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E,A] =
      this match {
        case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
        case Or(left, right) => left(a) match {
          case Valid(a) => Valid(a)
          case Invalid(e) => right(a) match {
            case Valid(a) => Valid(a)
            case Invalid(e2) => Invalid(e |+| e2)
          }
        }
        case Pure(func) => func(a)
      }
  }
  object Check {
    final case class And[E,A](left: Check[E,A], right: Check[E,A]) extends Check[E,A]
    final case class Or[E,A](left: Check[E,A], right: Check[E,A]) extends Check[E,A]
    final case class Pure[E,A](func: A => Validated[E,A]) extends Check[E,A]
    def pure[E,A](f: A => Validated[E,A]): Check[E,A] = Pure(f)
  }
}



object CheckFirstApp extends App {
  import CheckStuff._

  val a: CheckF[List[String], Int] = CheckF { v =>
    if(v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

  val b: CheckF[List[String], Int] = CheckF { v =>
    if(v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

  val check: CheckF[List[String], Int] = a and b

  println(check(5))
  println(check(0))

  val aa: CheckF[Nothing, Int] = CheckF(v => v.asRight)
  val bb: CheckF[Nothing, Int] = CheckF(v => v.asRight)
  // could not find implicit value for parameter s: cats.Semigroup[Nothing]
  // val ccheck = aa and bb

  // ADT usage
  val a2: Check[List[String], Int] = Check.pure { v =>
    if (v > 2) valid(v) else invalid(List("Must be > 2"))
  }
  val b2: Check[List[String], Int] = Check.pure { v =>
    if (v < - 2) valid(v) else invalid(List("Must be < -2"))
  }
  val check2: Check[List[String], Int] = a2 and b2
  println(check2(5))
  println(check2(0))
  println(a2(3))
  println((a2 or b2)(3))
  println((a2 or b2)(-3))
  println((a2 or b2)(0))
}
