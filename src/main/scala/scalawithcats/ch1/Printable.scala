package scalawithcats.ch1

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    def format(value: Int): String = value.toString
  }

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(value: String): String = value
  }
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)
  def print[A](value: A)(implicit p: Printable[A]): Unit =
    println(format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    def print(implicit p: Printable[A]): Unit = println(p.format(value))
  }
}

final case class Cat(name: String, age: Int, color: String)

object PrintableApp extends App {
  val cat = Cat("Puss in boots", 2, "Orange")

  import PrintableInstances._

  implicit val catPrinter = new Printable[Cat] {
    def format(value: Cat): String = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"${name} is ${age} year(s) old and is ${color}"
    }
  }

  import PrintableSyntax._
  println(cat.format)
}
