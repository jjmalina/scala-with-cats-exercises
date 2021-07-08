package scalawithcats.ch2

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid
}

object Main extends App {
  // Exercise 2.3
  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean): Boolean = x && y
    def empty = true
  }
  implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean): Boolean = x || y
    def empty = true
  }
  implicit val booleanEitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
    def empty = false
  }
  implicit val booleanNeitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
    def empty = true
  }

  // Exercise 2.4
  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def combine(a: Set[A], b: Set[A]): Set[A] = a union b
    def empty = Set.empty[A]
  }
  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
  }
  implicit def symmetricDifferenceMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def combine(x: Set[A], y: Set[A]): Set[A] = (x diff y) union (y diff x)
    def empty = Set.empty[A]
  }
}
