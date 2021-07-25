package scalawithcats.ch6

import cats.Semigroupal
import cats.instances.list._ // for Semigroupal
import cats.syntax.apply._

object SemigroupalApp extends App {
  println(Semigroupal[List].product(List(1, 2), List(3, 4)))
  println((List(1, 2), List(3, 4)).tupled)
  //  Why does product for List produce the Cartesian product?
  /*
    because it uses flatmap so each item in the first list is tupled
    with list 2
  */

}
