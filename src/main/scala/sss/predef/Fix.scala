package sss.predef

object Fix {
  import cats._

  /**
    * DSL for defining recursive computations
    */
  abstract class Sym[T[_]: Monad] extends syntax.AllSyntax {
    type τ[A] = T[A]

    def done[A]: A => τ[A]
    def suspend[A](ta: => τ[A]): τ[A]
    def run[A](ta: => τ[A]): A

    /**
      * the implicit definition here allows us to use 'suspend' in for
      * comprehensions. interestingly, however things breakdown if we explicitly
      * type it, because the compiler tries to resolve it using itself
      * ("ambiguous implicit values")
      */
    @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.ExplicitImplicitTypes"))
    implicit def monadInstance = implicitly[Monad[T]]
  }
  /**
    * below allows us to use something like x.done[T] for some Sym[T]
    */
  implicit final class SymDone[A](val x: A) extends AnyVal {
    def done[T[_]: Sym]: T[A] = implicitly[Sym[T]].done(x)
  }
  /**
    * below allows us to use _.recurse to run the computation. we we explicitly
    * don't want to have 'suspend' here because it detracts from the
    * stack-safety of the interpretors
    */
  implicit final class SymOps[T[_]: Sym, A](x: T[A]) {
    def recurse: A = implicitly[Sym[T]].run(x)
  }

  /**
    * generic fixpoint operator. this operator is stack-safe modulo the
    * stack-safety of the interpreters
    */
  final def apply[T[_], A, B](f: (A => T[B]) => (A => T[B]))(implicit e1: Sym[T]): A => T[B] = {
    f((x: A) => Fix(f)(e1)(x))
  }

  // {{{ recursive interpreters

  import cats.free, free._
  type Trampoline[A] = free.Trampoline[A]
  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.NonUnitStatements"))
  implicit def TrampolineAsSym: Sym[Trampoline] = new Sym[Trampoline] {
    import std.function._ // get Comonad[Function0] instance for 'run'
    def done[A] = Trampoline.done[A]
    def suspend[A](ta: => τ[A]) = Trampoline.suspend[A](ta)
    def run[A](ta: => τ[A]): A = ta.run
  }

  import scala.util.control.TailCalls
  type TailRec[+A] = TailCalls.TailRec[A]
  implicit def TailRecAsMonad: Monad[TailRec] = new Monad[TailRec] {
    def pure[A](x: A) = TailCalls.done(x)
    def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]) = fa.flatMap(f)
  }
  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.NonUnitStatements"))
  implicit def TailRecAsSym: Sym[TailRec] = new Sym[TailRec] {
    def done[A] = TailCalls.done[A]
    def suspend[A](ta: => τ[A]) = TailCalls.tailcall[A](ta)
    def run[A](ta: => τ[A]) = ta.result
  }

  // }}}

  // {{{ some other variants - deprecated

  import scala.annotation.tailrec

  @deprecated("This function is not stack-safe", "forever")
  def fixSimple[A, B](f: (A => B) => (A => B)): A => B = {
    f((x: A) => fixSimple(f)(x))
  }

  // tail-recursive version of fixpoint, by doubling composition.
  // NOTE: this doesn't work if 'f' is curried. i.e., 'B' cannot be of the
  // form 'C => ...'. the reason is that in that case FixException escapes
  // the context. if 'B' is not a concrete value (but is a function) then
  // 'FixException' is only thrown when it gets applied to something, at
  // which point it's too late (unless we override the .apply function, by
  // creating a 'Fix' object?)
  object Exception {
    final case class FixException() extends RuntimeException
    @deprecated("This function is not stack-safe", "forever")
    @tailrec final def apply[A, B](f: (A => B) => (A => B))(x: A): B = try {
      f(_ => throw FixException())(x)
    } catch {
      case e: FixException => Exception(f andThen f)(x)
    }
  }

  // TODO: try other variants from
  // <http://okmij.org/ftp/Computation/fixed-point-combinators.html#Self->

  // }}}
}
