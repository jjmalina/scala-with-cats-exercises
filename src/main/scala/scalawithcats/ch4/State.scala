package scalawithcats.ch4

import cats.data.State
import cats.syntax.applicative._ // for pure

object StateUsage {
  type CalcState[A] = State[List[Int], A]
  def evaluate(left: Int, right: Int, operator: String): Int = operator match {
    case "+" => left + right
    case "-" => left - right
    case "*" => left * right
    case "/" => left / right
    case _ => throw new Exception("Invalid operator")
  }
  def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] { oldStack =>
    val isOperator = "+-/*".contains(sym)
    if (isOperator) {
      val result = oldStack match {
        case second :: first :: _ => evaluate(first, second, sym)
        case _ => throw new Exception("not enough operands to evaluate expression")
      }
      (result :: oldStack.slice(2, oldStack.length), result)
    } else {
      (sym.toInt :: oldStack, sym.toInt)
    }
  }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (s, item) =>
      s.flatMap(_ => evalOne(item))
    }

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value
}

object StateApp extends App {
  import StateUsage._

  println(evalOne("42").runA(Nil).value)

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans
  println(program.runA(Nil).value)

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
  println(multistageProgram.runA(Nil).value)
  println(evalInput("1 2 + 3 *"))
}
