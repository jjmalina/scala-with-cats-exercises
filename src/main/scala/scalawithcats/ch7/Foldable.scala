package scalawithcats.ch7

import scala.math.Numeric
import cats.Foldable
import cats.instances.list._ // for Foldable
import cats.instances.option._ // for Foldable

object ListMethods {
  // 7.1.3
  def map[A,B](l: List[A])(f: A => B): List[B] =
    l.foldRight(List.empty[B])((a, b) => f(a) :: b)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    l.foldRight(List.empty[B])((a, b) => f(a) ::: b)

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else List.empty[A])

  def sum[A](l: List[A])(implicit numeric: Numeric[A]): A =
    l.foldRight(numeric.zero)(numeric.plus)
}

object FoldableApp extends App {

  // 7.1.2  Exercise: Reflecting on Folds
  val l = List(1, 2, 3)
  // _ :: _ doesnt work because :: is not a method of int
  println(l.foldLeft(List.empty[Int])((acc, item) => item :: acc)) // reverses list
  println(l.foldRight(List.empty[Int])(_ :: _))  // same order as original

  // 7.1.3
  println("7.1.3")
  import ListMethods._
  println(map(List(1, 2, 3))(_ + 1))
  println(flatMap(List(1, 2, 3))(a => List(a, a + 1)))
  println(filter(l)(_ % 2 == 0))
  println(sum(l))

  Foldable[List].foldLeft(l, 0)(_ + _)
  val maybeInt = Option(123)
  Foldable[Option].foldLeft(maybeInt, 10)(_ * _)
}
