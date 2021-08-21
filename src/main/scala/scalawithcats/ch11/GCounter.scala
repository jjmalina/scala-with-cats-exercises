package scalawithcats.ch11

import cats.Monoid
import cats.kernel.CommutativeMonoid
import cats.instances.list._ // for Monoid
import cats.instances.map._ // for Monoid
import cats.syntax.semigroup._ // for |+|
import cats.syntax.foldable._ // for combineAll

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BSLInstances {
  implicit val intBSL = new BoundedSemiLattice[Int] {
    def combine(a1: Int, a2: Int): Int = a1 max a2
    def empty = 0
  }

  implicit def setBSL[A] = new BoundedSemiLattice[Set[A]] {
    def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 | a2
    def empty: Set[A] = Set[A]()
  }
}

final case class GCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int) = {
    GCounter(
      counters + (machine -> (counters.getOrElse(machine, 0) + amount))
    )
  }

  def merge(that: GCounter): GCounter =
    GCounter(that.counters ++ this.counters.map {
      case (k, v) => k -> (v max that.counters.getOrElse(k, 0))
    })

  def total: Int = counters.values.sum
}

final case class GenericGCounter[A](counters: Map[String, A]) {

  def increment(machine: String, amount: A)(implicit m: Monoid[A]) = {
    GenericGCounter(
      counters.combine(Map(
        machine -> (counters.getOrElse(machine, m.empty) |+| amount)
      ))
    )
  }

  def merge(that: GenericGCounter[A])(implicit bsl: BoundedSemiLattice[A]): GenericGCounter[A] = {
    GenericGCounter(that.counters ++ this.counters.map {
      case (k, v) => k -> (v |+| that.counters.getOrElse(k, bsl.empty))
    })
  }

  def total(implicit m: CommutativeMonoid[A]): A =
    counters.values.toList.combineAll
}

trait GCounterTClass[F[_,_], K, V] {
  def increment(f: F[K,V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K,V]
  def merge(f1: F[K,V], f2: F[K,V])(implicit b: BoundedSemiLattice[V]): F[K,V]
  def total(f: F[K,V])(implicit m: CommutativeMonoid[V]): V
}
object GCounterTClass {
  def apply[F[_,_], K, V](implicit counter: GCounterTClass[F,K,V]) = counter
}

object GCounterTClassInstances {
  implicit def gCounterMapInstance[K,V] = new GCounterTClass[Map, K, V] {
    def increment(f: Map[K,V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K,V] =
      f.combine(Map(
        k -> (f.getOrElse(k, m.empty) |+| v)
      ))

    def merge(f1: Map[K,V], f2: Map[K,V])(implicit b: BoundedSemiLattice[V]): Map[K,V] =
      f1 |+| f2

    def total(f: Map[K,V])(implicit m: CommutativeMonoid[V]): V =
      f.values.toList.combineAll
  }
}

object GCounterUsage {
  import GCounterTClassInstances._
  import BSLInstances._

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

  val counter = GCounterTClass[Map, String, Int]

  val merged = counter.merge(g1, g2)
  val total = counter.total(merged)
}

trait KeyValueStore[F[_,_]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)
  def values[K, V](f: F[K, V]): List[V]
}

object KVInstances {
  implicit val mapKVInstance = new KeyValueStore[Map] {
    def put[K,V](f: Map[K, V])(k: K, v: V): Map[K, V] =
      f + (k -> v)

    def get[K,V](f: Map[K, V])(k: K): Option[V] =
      f.get(k)

    override def getOrElse[K,V](f: Map[K, V])(k: K, default: V): V =
      get(f)(k).getOrElse(default)

    def values[K,V](f: Map[K, V]): List[V] = f.values.toList
  }
}
