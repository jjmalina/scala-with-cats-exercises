package scalawithcats.ch4

import cats.Monad
// import cats.syntax.monad._ // for iterateWhileM
import cats.syntax.flatMap._ // for flatMap
import cats.syntax.functor._ // for map

object MonadUsage {
  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    def flatMap[A,B](ta: Tree[A])(fn: A => Tree[B]): Tree[B] =
      ta match {
        case Branch(left, right) => branch(flatMap(left)(fn), flatMap(right)(fn))
        case Leaf(value) => fn(value)
      }
    def pure[A](a: A): Tree[A] = leaf(a)

    def tailRecM[A,B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(fn(a)) {
        case Left(value) => tailRecM(value)(fn)
        case Right(value) => Leaf(value)
      }
  }
}

object MonadApp extends App {
  import MonadUsage._

  println(branch(leaf(100), leaf(200))
    .flatMap(x => branch(leaf(x - 1), leaf(x + 1))))

  val b = for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c
  println(b)
}
