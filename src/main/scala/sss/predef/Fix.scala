package sss.predef

/**
  * generic stack-safe recursive computations. see 'Fix.Sym' for the DSL used to
  * define said computations. see 'Fix.apply' to see how to run such
  * computations.
  */
object Fix {
  import cats._

  /**
    * DSL for defining recursive computations
    */
  abstract class Sym[T[_]](implicit val m: Monad[T]) extends syntax.AllSyntax { self =>
    type τ[A] = T[A]

    def done[A]: A => τ[A]
    def suspend[A](ta: => τ[A]): τ[A]
    /**
      * 'run' is private to ensure that we can control access to
      * it. specifically, the only way to 'run' a recursive computation is via
      * 'FixOps.recurse' whose construction is only possible via 'Fix.apply'.
      *
      *  why do we care? because we want to make it harder to accidentally use
      *  'run' in the process of describing/defining a recursive computation (or
      *  it detracts from the stack-safety).
      */
    private[Fix] def run[A](ta: => τ[A]): A

    /**
      * the implicit definition here allows us to use 'suspend' in for
      * comprehensions, by bringing the elements of pertinent 'Sym[T]' into
      * scope.
      */
    implicit def monadInstance: Monad[τ] = m
  }
  /**
    * below allows us to use something like x.done[T] for some Sym[T]
    */
  implicit final class SymDone[A](val x: A) extends AnyVal {
    def done[T[_]: Sym]: T[A] = implicitly[Sym[T]].done(x)
  }
  /**
    * the only way to construct a 'FixOps' instance is using 'Fix.apply', and
    * the only thing you can do with the 'FixOps' instance is to call
    * '_.recurse' (which runs the recursive computation)
    */
  final case class FixOps[T[_]: Sym, A] private[Fix] (private val x: T[A]) {
    def recurse: A = implicitly[Sym[T]].run(x)
  }

  /**
    * generic fixpoint operator. this operator is stack-safe modulo the
    * stack-safety of the interpreters. additionally, the return type ensures
    * that the only thing you can do with the result is to call 'recurse' on it
    */
  final def apply[T[_]: Sym, A, B](f: (A => T[B]) => (A => T[B])): A => FixOps[T, B] = {
    def fix(f: (A => T[B]) => (A => T[B]))(implicit e1: Sym[T]): A => T[B] = {
      f((x: A) => fix(f)(e1)(x))
    }
    // call the fixpoint function and then wrap it in a FixOps instance
    fix(f) andThen FixOps[T, B]
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
