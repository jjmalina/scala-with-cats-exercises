package scalawithcats.ch1

import cats.Show
import cats.implicits._


object ShowCatsApp extends App {
  val cat = Cat("Puss in boots", 2, "Orange")

  implicit val showCat = new Show[Cat] {
    def show(value: Cat): String = {
      val name = cat.name.show
      val age = cat.age.show
      val color = cat.color.show
      s"${name} is a ${age} year-old ${color} cat"
    }
  }

  /* OR
  implicit val catShow: Show[Cat] = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"${name} is a ${age} year-old ${color} cat"
  }
  */

  println(cat.show)
}
