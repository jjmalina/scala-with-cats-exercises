package scalawithcats.ch4

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._ // for tell

object WriterUsage {
  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) = try body finally Thread.sleep(100)
  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if(n == 0) 1.pure[Logged] else factorial(n - 1).map(_ * n)
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }
}

object WriterApp extends App {
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._
  import WriterUsage._

  val (log, res) = factorial(5).run
  println(log)
  println(res)
  val v = Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )).map(_.map(_.written)), 5.seconds)
  println(v)
}
