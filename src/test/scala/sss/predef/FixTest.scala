package sss.predef

import sss.tags._
import org.scalatest._

// suppress deprecation warnings over narrow scope
// <https://issues.scala-lang.org/browse/SI-7934>
@deprecated("", "")
class FixTest extends FreeSpec {

  "A Fixpoint operator should" - {
    "not blow the stack" - {
      // we'll use the ackermann function as testcase
      // <https://en.wikipedia.org/wiki/Ackermann_function>
      def acktabs(ack: ((Int, Int)) => Int)(in: (Int, Int)): Int = {
        val (m, n) = in
        if (m <= 0) n + 1
        else if (n <= 0) ack((m - 1, 1))
        else ack((m - 1, ack((m, n - 1))))
      }
      val alot = (3, 12)
      def verifyResult(actual: Any): Unit = assertResult(32765)(actual)

      "Fix.apply" - {
        import Fix._

        // for the generic interface we need to transform our recursive
        // definition to use our recursion primitives. this is similar in spirit
        // to the idea presented in
        // <http://eed3si9n.com/herding-cats/stackless-scala-with-free-monads.html>
        def acktabs[T[_]: Sym](ack: ((Int, Int)) => T[Int])(in: (Int, Int)): T[Int] = {
          // bring the DSL into scope
          val e1 = implicitly[Sym[T]]
          import e1._
          // define the computation
          val (m, n) = in
          if (m <= 0) done(n + 1)
          else if (n <= 0) suspend(ack((m - 1, 1)))
          else for {
            a <- suspend(ack((m, n - 1)))
            b <- suspend(ack((m - 1, a)))
          } yield b
        }

        "Fix[Trampoline]" taggedAs (SlowTest) in {
          verifyResult {
            Fix(acktabs[Trampoline]).apply(alot).recurse
          }
        }
        "Fix[TailRec]" taggedAs (SlowTest) in {
          verifyResult {
            Fix(acktabs[TailRec]).apply(alot).recurse
          }
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
