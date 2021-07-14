package scalawithcats.ch4

import cats.Id

object IdMethods {
  def pure[A](a: Id[A]): Id[A] = a
  def map[A,B](a: Id[A])(f: A => B): Id[B] = f(a)
  def flatMap[A,B](a: Id[A])(f: A => Id[B]): Id[B] =
    map(a)(f)
}
