package scalawithcats.ch10

import cats.Semigroup
import cats.syntax.semigroup._ // for |+|
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.apply._
import cats.syntax.validated._ // for valid and invalid

object ChecksDSL {
  sealed trait Predicate[E,A] {
    import Predicate._

    def and(that: Predicate[E,A]): Predicate[E,A] = And(this, that)

    def or(that: Predicate[E,A]): Predicate[E,A] = Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
      case Pure(func) => func(a)
      case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) => left(a) match {
        case Valid(_) => Valid(a)
        case Invalid(e1) => right(a) match {
          case Valid(_) => Valid(a)
          case Invalid(e2) => Invalid(e1 |+| e2)
        }
      }
    }
  }
  object Predicate {
    final case class And[E,A](
      left: Predicate[E,A], right: Predicate[E,A]) extends Predicate[E,A]

    final case class Or[E,A](
      left: Predicate[E,A], right: Predicate[E,A]
    ) extends Predicate[E,A]

    final case class Pure[E,A](func: A => Validated[E,A]) extends Predicate[E,A]

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] =
      Pure(f)

    def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
      Pure(a => if(fn(a)) a.valid else err.invalid)
  }
  sealed trait Check[E,A,B] {
    import Check._

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E,B]

    def map[C](func: B => C): Check[E, A, C] =
      Map(this, func)

    def flatMap[C](func: B => Check[E,A,C]): Check[E,A,C] =
      FlatMap(this, func)

    def andThen[C](that: Check[E,B,C]): Check[E,A,C] =
      AndThen(this, that)
  }
  object Check {
    final case class Map[E,A,B,C](
      check: Check[E,A,B],
      func: B => C) extends Check[E,A,C] {

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E,C] =
        check(a).map(func)
    }

    final case class Pure[E,A,B](func: A => Validated[E,B]) extends Check[E,A,B] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E,B] =
        func(a)
    }

    final case class PurePredicate[E,A](
      pred: Predicate[E,A]) extends Check[E,A,A] {

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
        pred(a)
    }

    final case class FlatMap[E,A,B,C](
      check: Check[E,A,B],
      func: B => Check[E,A,C]) extends Check[E,A,C] {

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E,C] =
        check(a).withEither(_.flatMap(b => func(b)(a).toEither))
    }

    final case class AndThen[E,A,B,C](
      check1: Check[E,A,B],
      check2: Check[E,B,C]) extends Check[E,A,C] {

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E,C] =
        check1(a).withEither(_.flatMap(b => check2(b).toEither))
    }

    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
      PurePredicate(pred)

    def apply[E,A,B](func: A => Validated[E,B]): Check[E,A,B] =
      Pure(func)
  }
}

object PredicateCombinators {
  import cats.data.NonEmptyList
  import ChecksDSL._

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n
    )

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit)
    )

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char)
    )

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1
    )
}

object UserChecks {
  import ChecksDSL._
  import PredicateCombinators._

  val usernameCheck = Check(longerThan(3) and alphanumeric)

  val leftEmailFragmentValidCheck = Check(longerThan(0))
  val rightEmailFragmentCheck = Check(longerThan(3) and contains('.'))

  val splitEmail: Check[Errors, String, (String, String)] = Check(_.split('@') match {
    case Array(name, domain) => (name, domain).validNel[String]
    case _ => "Must contain a single @ character".invalidNel[(String, String)]
  })

  val joinEmail: Check[Errors, (String, String), String] = Check {
    case (l, r) => (
      leftEmailFragmentValidCheck(l),
      rightEmailFragmentCheck(r)
    ).mapN(_ + "@" + _)
  }

  val checkEmail = splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(username: String, email: String): Validated[Errors, User] =
    (usernameCheck(username), checkEmail(email)).mapN(User)
}
