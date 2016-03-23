package sss.predef

import sss.tags._
import org.scalatest._

// suppress deprecation warnings over narrow scope
// <https://issues.scala-lang.org/browse/SI-7934>
@deprecated("", "")
class FixTest extends FreeSpec {
  // en.wikipedia.org/wiki/Ackermann_function
  def acktabs(ack: ((Int, Int)) => Int)(in: (Int, Int)): Int = {
    val (m, n) = in
    if (m <= 0) n + 1
    else if (n <= 0) ack((m - 1, 1))
    else ack((m - 1, ack((m, n - 1))))
  }

  val alot = (3, 12)
  def verifyResult(actual: Any): Unit = assertResult(32765)(actual)

  "A Fixpoint operator should" - {
    "not blow the stack" - {
      "Fix.apply" taggedAs (SlowTest) in {
        import Fix.instances._
        // <http://eed3si9n.com/herding-cats/stackless-scala-with-free-monads.html>
        def acktabs(ack: ((Int, Int)) => Fix.T[Int])(in: (Int, Int)): Fix.T[Int] = {
          val (m, n) = in
          if (m <= 0) Fix.T.done(n + 1)
          else if (n <= 0) Fix.T.suspend(ack((m - 1, 1)))
          else for {
            a <- Fix.T.suspend(ack((m, n - 1)))
            b <- Fix.T.suspend(ack((m - 1, a)))
          } yield b
        }
        verifyResult {
          Fix(acktabs)(alot).run
        }
      }
      "Fix.exception" taggedAs (SlowTest) in pendingUntilFixed {
        try {
          verifyResult {
            Fix.Exception(acktabs)(alot)
          }
        } catch {
          case _: StackOverflowError => fail()
        }
      }
      "Fix.fixSimple" taggedAs (SlowTest) in pendingUntilFixed {
        try {
          verifyResult {
            Fix.fixSimple(acktabs)(alot)
          }
        } catch {
          case _: StackOverflowError => fail()
        }
      }
    }
  }

}
