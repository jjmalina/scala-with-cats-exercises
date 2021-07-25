package scalawithcats.ch9

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import cats.Monoid
import cats.syntax.semigroup._ // for |+|
import cats.instances.future._ // for Monad and Monoid
import cats.instances.list._ // for Traverse
import cats.syntax.traverse._ // for sequence
import cats.instances.vector._ // for Foldable and Traverse
import cats.syntax.foldable._ // for combineAll and foldMap

object FoldableMethods {
  def foldMap[A, B : Monoid](v: Vector[A])(f: A => B): B =
    v.foldLeft(Monoid[B].empty)(_ |+| f(_))

  def parallelFoldMap[A, B : Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values.grouped(groupSize).map(
      v => Future(foldMap(v)(f))
    ).toList.sequence.map(Monoid[B].combineAll(_))
  }

  def parallelFoldMapViaCats[A, B : Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values.grouped(groupSize).toVector.traverse(
      group => Future(group.toVector.foldMap(f))).map(_.combineAll)
  }
}

object FoldableApp extends App {
  import FoldableMethods._
  import cats.instances.int._ // for Monoid
  println(foldMap(Vector(1, 2, 3))(identity))
  // res1: Int = 6
  import cats.instances.string._ // for Monoid
  // Mapping to a String uses the concatenation monoid:
  println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
  // Mapping over a String to produce a String:
  println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))
  // res3: String = "HELLO WORLD!"

  println(Await.result(parallelFoldMap((1 to 100).toVector)(identity), 1.second))

  val result: Future[Int] = parallelFoldMap((1 to 1000000).toVector)(identity)
  println(Await.result(result, 1.second))

  val future: Future[Int] = parallelFoldMapViaCats((1 to 1000).toVector)(_ * 1000)
  println(Await.result(future, 1.second))
}
