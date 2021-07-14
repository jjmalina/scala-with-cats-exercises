package scalawithcats.ch4

import cats.Eval
import scala.annotation.tailrec

object EvalUsage {
  // my first implementation
  def foldRightTailRec[A,B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    @tailrec
    def go(a: List[A], eAcc: Eval[B]): Eval[B] = {
      a match {
        case Nil => eAcc
        case head :: tl =>
          go(tl, eAcc.map(fn(head, _)))
      }
    }
    go(as, Eval.now(acc)).value
  }
  // solutions
  def foldRightEval[A,B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case Nil => acc
      case head :: tl => Eval.defer(fn(head, foldRightEval(tl, acc)(fn)))
    }
  def foldRight[A,B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc))((a, b) => b.map(fn(a, _))).value
}

object EvalApp extends App {
  import EvalUsage._
  // runs out of memory if you try to do 1 to 100M
  println(foldRight((1 to 100000).toList, 0L)(_ + _))
  println(foldRightTailRec((1 to 100000).toList, 0L)(_ + _))
}
