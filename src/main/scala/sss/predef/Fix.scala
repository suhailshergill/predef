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
  // <https://github.com/puffnfresh/wartremover/issues/223>. the other option
  // would be to use the equivalent "private[this] val m" instead
  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.ExplicitImplicitTypes"))
  abstract class Sym[T[_]](implicit m: Monad[T]) extends syntax.AllSyntax { self =>
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

// no val
/*
public abstract class sss.predef.Fix$Sym<T> implements cats.syntax.AllSyntax {
  private final cats.Monad<T> m;
  private volatile cats.syntax.StreamingSyntax$$percent$colon$colon$ $percent$colon$colon$module;
  public <F, A> F coproductSyntax(F);
  public <A> A validatedIdSyntax(A);
  public <A> A xorIdSyntax(A);
  public <F, A> cats.syntax.TraverseOps<F, A> traverseSyntax(F, cats.Traverse<F>);
  public <F, G, A> cats.syntax.NestedTraverseOps<F, G, A> nestedTraverseSyntax(F, cats.Traverse<F>);
  public <FA> cats.syntax.TraverseOps<java.lang.Object, java.lang.Object> traverseSyntaxU(FA, cats.Unapply<cats.Traverse, FA>);
  public <F, A, B> cats.syntax.StrongOps<F, A, B> strongSyntax(F, cats.functor.Strong<F>);
  private cats.syntax.StreamingSyntax$$percent$colon$colon$ $percent$colon$colon$lzycompute();
  public cats.syntax.StreamingSyntax$$percent$colon$colon$ $percent$colon$colon();
  public <A> cats.syntax.StreamingSyntax$StreamingOps<A> streamingOps(scala.Function0<cats.data.Streaming<A>>);
  public <F, A, B> cats.syntax.SplitOps<F, A, B> splitSyntax(F, cats.arrow.Split<F>);
  public <T> cats.Show$Ops<T> toShowOps(T, cats.Show<T>);
  public <FA> cats.SemigroupK$Ops<java.lang.Object, java.lang.Object> semigroupSyntaxU(FA, cats.Unapply<cats.SemigroupK, FA>);
  public <F, A> cats.SemigroupK$Ops<F, A> toSemigroupKOps(F, cats.SemigroupK<F>);
  public <F, G, A> cats.syntax.NestedReducibleOps<F, G, A> nestedReducibleSyntax(F, cats.Reducible<F>);
  public <FA> cats.Reducible$Ops<java.lang.Object, java.lang.Object> foldableSyntaxU(FA, cats.Unapply<cats.Reducible, FA>);
  public <F, C> cats.Reducible$Ops<F, C> toReducibleOps(F, cats.Reducible<F>);
  public <F, A, B> cats.syntax.ProfunctorOps<F, A, B> profunctorSyntax(F, cats.functor.Profunctor<F>);
  public <A> cats.syntax.OrderOps<A> orderSyntax(A, algebra.Order<A>);
  public <A> cats.syntax.PartialOrderOps<A> partialOrderSyntax(A, algebra.PartialOrder<A>);
  public final <A> scala.Option<A> none();
  public final <A> A optionIdSyntax(A);
  public final <A> scala.Option<A> optionSyntax(scala.Option<A>);
  public <FA> cats.MonadFilter$Ops<java.lang.Object, java.lang.Object> monadFilterSyntaxU(FA, cats.Unapply<cats.MonadFilter, FA>);
  public <F, A> cats.MonadFilter$Ops<F, A> toMonadFilterOps(F, cats.MonadFilter<F>);
  public <F, G, A> cats.syntax.NestedMonadCombineOps<F, G, A> nestedMonadCombineSyntax(F, cats.MonadCombine<F>);
  public <A> scala.collection.immutable.List<A> listSyntax(scala.collection.immutable.List<A>);
  public <FA> cats.functor.Invariant$Ops<java.lang.Object, java.lang.Object> invariantSyntaxU(FA, cats.Unapply<cats.functor.Invariant, FA>);
  public <F, A> cats.functor.Invariant$Ops<F, A> toInvariantOps(F, cats.functor.Invariant<F>);
  public <A> cats.syntax.GroupOps<A> groupSyntax(A, algebra.Group<A>);
  public <A> cats.syntax.SemigroupOps<A> semigroupSyntax(A, algebra.Semigroup<A>);
  public <FA> cats.Functor$Ops<java.lang.Object, java.lang.Object> functorSyntaxU(FA, cats.Unapply<cats.Functor, FA>);
  public <F, A> cats.Functor$Ops<F, A> toFunctorOps(F, cats.Functor<F>);
  public <F, G, A> cats.syntax.NestedFoldableOps<F, G, A> nestedFoldableSyntax(F, cats.Foldable<F>);
  public <FA> cats.Foldable$Ops<java.lang.Object, java.lang.Object> foldableSyntaxU(FA, cats.Unapply<cats.Foldable, FA>);
  public <F, C> cats.Foldable$Ops<F, C> toFoldableOps(F, cats.Foldable<F>);
  public <F, A> cats.syntax.FlatMapOps<F, A> flatMapSyntax(F, cats.FlatMap<F>);
  public <F, A> cats.syntax.FlattenOps<F, A> flattenSyntax(F, cats.FlatMap<F>);
  public <F> cats.syntax.IfMOps<F> ifMSyntax(F, cats.FlatMap<F>);
  public <FA> cats.syntax.FlatMapOps<java.lang.Object, java.lang.Object> flatMapSyntaxU(FA, cats.Unapply<cats.FlatMap, FA>);
  public <A> cats.syntax.EqOps<A> eqSyntax(A, algebra.Eq<A>);
  public <A, B> scala.util.Either<A, B> eitherSyntax(scala.util.Either<A, B>);
  public <FA> cats.functor.Contravariant$Ops<java.lang.Object, java.lang.Object> contravariantSyntaxU(FA, cats.Unapply<cats.functor.Contravariant, FA>);
  public <F, A> cats.functor.Contravariant$Ops<F, A> toContravariantOps(F, cats.functor.Contravariant<F>);
  public <F, A, B> cats.syntax.ComposeOps<F, A, B> composeSyntax(F, cats.arrow.Compose<F>);
  public <FA> cats.Comonad$Ops<java.lang.Object, java.lang.Object> comonadSyntaxU(FA, cats.Unapply<cats.Comonad, FA>);
  public <F, A> cats.Comonad$Ops<F, A> toComonadOps(F, cats.Comonad<F>);
  public <FA> cats.CoflatMap$Ops<java.lang.Object, java.lang.Object> coflatMapSyntaxU(FA, cats.Unapply<cats.CoflatMap, FA>);
  public <F, A> cats.CoflatMap$Ops<F, A> toCoflatMapOps(F, cats.CoflatMap<F>);
  public <F, A> cats.syntax.CartesianOps<F, A> cartesianSyntax(F, cats.Cartesian<F>);
  public <FA> cats.syntax.CartesianOps<java.lang.Object, java.lang.Object> cartesianSyntaxU(FA, cats.Unapply<cats.Cartesian, FA>);
  public <F, A, B> cats.syntax.BifunctorOps<F, A, B> bifunctorSyntax(F, cats.functor.Bifunctor<F>);
  public <F, A> cats.Apply$Ops<F, A> applySyntax(F, cats.Apply<F>);
  public <FA> cats.Apply$Ops<java.lang.Object, java.lang.Object> applySyntaxU(FA, cats.Unapply<cats.Apply, FA>);
  public abstract <A> scala.Function1<A, T> done();
  public abstract <A> T suspend(scala.Function0<T>);
  public abstract <A> A run(scala.Function0<T>);
  public cats.Monad<java.lang.Object> monadInstance();
  public sss.predef.Fix$Sym(cats.Monad<T>);
}
*/

// w/ val
/*
public abstract class sss.predef.Fix$Sym<T> implements cats.syntax.AllSyntax {
  private final cats.Monad<T> m;
  private volatile cats.syntax.StreamingSyntax$$percent$colon$colon$ $percent$colon$colon$module;
  public <F, A> F coproductSyntax(F);
  public <A> A validatedIdSyntax(A);
  public <A> A xorIdSyntax(A);
  public <F, A> cats.syntax.TraverseOps<F, A> traverseSyntax(F, cats.Traverse<F>);
  public <F, G, A> cats.syntax.NestedTraverseOps<F, G, A> nestedTraverseSyntax(F, cats.Traverse<F>);
  public <FA> cats.syntax.TraverseOps<java.lang.Object, java.lang.Object> traverseSyntaxU(FA, cats.Unapply<cats.Traverse, FA>);
  public <F, A, B> cats.syntax.StrongOps<F, A, B> strongSyntax(F, cats.functor.Strong<F>);
  private cats.syntax.StreamingSyntax$$percent$colon$colon$ $percent$colon$colon$lzycompute();
  public cats.syntax.StreamingSyntax$$percent$colon$colon$ $percent$colon$colon();
  public <A> cats.syntax.StreamingSyntax$StreamingOps<A> streamingOps(scala.Function0<cats.data.Streaming<A>>);
  public <F, A, B> cats.syntax.SplitOps<F, A, B> splitSyntax(F, cats.arrow.Split<F>);
  public <T> cats.Show$Ops<T> toShowOps(T, cats.Show<T>);
  public <FA> cats.SemigroupK$Ops<java.lang.Object, java.lang.Object> semigroupSyntaxU(FA, cats.Unapply<cats.SemigroupK, FA>);
  public <F, A> cats.SemigroupK$Ops<F, A> toSemigroupKOps(F, cats.SemigroupK<F>);
  public <F, G, A> cats.syntax.NestedReducibleOps<F, G, A> nestedReducibleSyntax(F, cats.Reducible<F>);
  public <FA> cats.Reducible$Ops<java.lang.Object, java.lang.Object> foldableSyntaxU(FA, cats.Unapply<cats.Reducible, FA>);
  public <F, C> cats.Reducible$Ops<F, C> toReducibleOps(F, cats.Reducible<F>);
  public <F, A, B> cats.syntax.ProfunctorOps<F, A, B> profunctorSyntax(F, cats.functor.Profunctor<F>);
  public <A> cats.syntax.OrderOps<A> orderSyntax(A, algebra.Order<A>);
  public <A> cats.syntax.PartialOrderOps<A> partialOrderSyntax(A, algebra.PartialOrder<A>);
  public final <A> scala.Option<A> none();
  public final <A> A optionIdSyntax(A);
  public final <A> scala.Option<A> optionSyntax(scala.Option<A>);
  public <FA> cats.MonadFilter$Ops<java.lang.Object, java.lang.Object> monadFilterSyntaxU(FA, cats.Unapply<cats.MonadFilter, FA>);
  public <F, A> cats.MonadFilter$Ops<F, A> toMonadFilterOps(F, cats.MonadFilter<F>);
  public <F, G, A> cats.syntax.NestedMonadCombineOps<F, G, A> nestedMonadCombineSyntax(F, cats.MonadCombine<F>);
  public <A> scala.collection.immutable.List<A> listSyntax(scala.collection.immutable.List<A>);
  public <FA> cats.functor.Invariant$Ops<java.lang.Object, java.lang.Object> invariantSyntaxU(FA, cats.Unapply<cats.functor.Invariant, FA>);
  public <F, A> cats.functor.Invariant$Ops<F, A> toInvariantOps(F, cats.functor.Invariant<F>);
  public <A> cats.syntax.GroupOps<A> groupSyntax(A, algebra.Group<A>);
  public <A> cats.syntax.SemigroupOps<A> semigroupSyntax(A, algebra.Semigroup<A>);
  public <FA> cats.Functor$Ops<java.lang.Object, java.lang.Object> functorSyntaxU(FA, cats.Unapply<cats.Functor, FA>);
  public <F, A> cats.Functor$Ops<F, A> toFunctorOps(F, cats.Functor<F>);
  public <F, G, A> cats.syntax.NestedFoldableOps<F, G, A> nestedFoldableSyntax(F, cats.Foldable<F>);
  public <FA> cats.Foldable$Ops<java.lang.Object, java.lang.Object> foldableSyntaxU(FA, cats.Unapply<cats.Foldable, FA>);
  public <F, C> cats.Foldable$Ops<F, C> toFoldableOps(F, cats.Foldable<F>);
  public <F, A> cats.syntax.FlatMapOps<F, A> flatMapSyntax(F, cats.FlatMap<F>);
  public <F, A> cats.syntax.FlattenOps<F, A> flattenSyntax(F, cats.FlatMap<F>);
  public <F> cats.syntax.IfMOps<F> ifMSyntax(F, cats.FlatMap<F>);
  public <FA> cats.syntax.FlatMapOps<java.lang.Object, java.lang.Object> flatMapSyntaxU(FA, cats.Unapply<cats.FlatMap, FA>);
  public <A> cats.syntax.EqOps<A> eqSyntax(A, algebra.Eq<A>);
  public <A, B> scala.util.Either<A, B> eitherSyntax(scala.util.Either<A, B>);
  public <FA> cats.functor.Contravariant$Ops<java.lang.Object, java.lang.Object> contravariantSyntaxU(FA, cats.Unapply<cats.functor.Contravariant, FA>);
  public <F, A> cats.functor.Contravariant$Ops<F, A> toContravariantOps(F, cats.functor.Contravariant<F>);
  public <F, A, B> cats.syntax.ComposeOps<F, A, B> composeSyntax(F, cats.arrow.Compose<F>);
  public <FA> cats.Comonad$Ops<java.lang.Object, java.lang.Object> comonadSyntaxU(FA, cats.Unapply<cats.Comonad, FA>);
  public <F, A> cats.Comonad$Ops<F, A> toComonadOps(F, cats.Comonad<F>);
  public <FA> cats.CoflatMap$Ops<java.lang.Object, java.lang.Object> coflatMapSyntaxU(FA, cats.Unapply<cats.CoflatMap, FA>);
  public <F, A> cats.CoflatMap$Ops<F, A> toCoflatMapOps(F, cats.CoflatMap<F>);
  public <F, A> cats.syntax.CartesianOps<F, A> cartesianSyntax(F, cats.Cartesian<F>);
  public <FA> cats.syntax.CartesianOps<java.lang.Object, java.lang.Object> cartesianSyntaxU(FA, cats.Unapply<cats.Cartesian, FA>);
  public <F, A, B> cats.syntax.BifunctorOps<F, A, B> bifunctorSyntax(F, cats.functor.Bifunctor<F>);
  public <F, A> cats.Apply$Ops<F, A> applySyntax(F, cats.Apply<F>);
  public <FA> cats.Apply$Ops<java.lang.Object, java.lang.Object> applySyntaxU(FA, cats.Unapply<cats.Apply, FA>);
  public cats.Monad<T> m();
  public abstract <A> scala.Function1<A, T> done();
  public abstract <A> T suspend(scala.Function0<T>);
  public abstract <A> A run(scala.Function0<T>);
  public cats.Monad<java.lang.Object> monadInstance();
  public sss.predef.Fix$Sym(cats.Monad<T>);
}
*/
