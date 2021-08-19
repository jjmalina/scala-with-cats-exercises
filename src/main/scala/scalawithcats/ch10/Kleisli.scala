package scalawithcats.ch10

import cats.Semigroup
import cats.syntax.semigroup._ // for |+|
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.apply._
import cats.syntax.validated._ // for valid and invalid
import cats.data.Kleisli

object KleisliChecksDSL {
  sealed trait Predicate[E,A] {
    import Predicate._

    def and(that: Predicate[E,A]): Predicate[E,A] = And(this, that)

    def or(that: Predicate[E,A]): Predicate[E,A] = Or(this, that)

    def run(implicit s: Semigroup[E]): A => Either[E, A] =
      (a: A) => this(a).toEither

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
}

object KleisliPredicateCombinators {
  import cats.data.NonEmptyList
  import KleisliChecksDSL._

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

object KleisliUserChecks {
  import KleisliChecksDSL._
  import KleisliPredicateCombinators._
  import cats.implicits._

  type Result[A] = Either[Errors, A]
  type Check[A,B] = Kleisli[Result,A,B]

  def check[A, B](func: A => Result[B]): Check[A,B] =
    Kleisli(func)

  def checkPred[A](pred: Predicate[Errors, A]): Check[A,A] =
    Kleisli[Result, A, A](pred.run)

  val usernameCheck = checkPred(longerThan(3) and alphanumeric)

  val leftEmailFragmentValidCheck = checkPred(longerThan(0))
  val rightEmailFragmentCheck = checkPred(longerThan(3) and contains('.'))

  val splitEmail: Check[String, (String, String)] = check(_.split('@') match {
    case Array(name, domain) => Right((name, domain))
    case _ => Left(error("Must contain a single @ character"))
  })

  val joinEmail: Check[(String, String), String] = check {
    case (l, r) => (
      leftEmailFragmentValidCheck(l),
      rightEmailFragmentCheck(r)
    ).mapN(_ + "@" + _)
  }

  val checkEmail = splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(username: String, email: String): Either[Errors, User] =
    (usernameCheck(username), checkEmail(email)).mapN(User)

  createUser("Noel", "noel@underscore.io")
  createUser("", "dave@underscore.io@io")
}
