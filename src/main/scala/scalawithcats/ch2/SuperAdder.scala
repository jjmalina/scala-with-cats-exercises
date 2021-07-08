package scalawithcats.ch2

import cats._
import cats.implicits._

object SuperAdder {
  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldRight(monoid.empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  def main() = {
    import cats.instances.int._
    add(List(1, 2, 3))
    import cats.instances.option._
    add(List(Some(1), None, Some(2), None , Some(3)))

    implicit val orderMonoid = new Monoid[Order] {
      def combine(a: Order, b: Order): Order =
        Order(a.totalCost + b.totalCost, a.quantity + b.quantity)
      def empty: Order = Order(0.0, 0.0)
    }
    add(List(Order(20.0, 15.0), Order(40.0, 5.0)))
  }
}
