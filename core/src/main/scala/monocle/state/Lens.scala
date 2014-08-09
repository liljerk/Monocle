package monocle.state

import monocle.{Lens, Optional, SimpleLens}

import scalaz.{IndexedState, State}


object optional {
  
  implicit def optionalToState[S, T, A, B](optional: Optional[S, T, A, B]): State[S, Option[A]] =
    optional.toState
  
  implicit def toStatePrismOps[S, T, A, B](optional: Optional[S, T, A, B]): StateOptionalOps[S, T, A, B] =
    new StateOptionalOps[S, T, A, B](optional)
}

final class StateOptionalOps[S, T, A, B](optional: Optional[S, T, A, B]){
  def toState: State[S, Option[A]] = State(s => (s, optional.getOption(s)))

  def updateState(f: A => B): IndexedState[S, T, Option[A]] =
    toState.leftMap( optional.modify(_, f) )

  def ~=(f: A => B): IndexedState[S, T, Option[A]] = updateState(f)

  def assign(value: B): IndexedState[S, T, Option[A]] =
    updateState(_ => value)

  def :=(value: B): IndexedState[S, T, Option[A]] = assign(value)
}


object lens {

  implicit def lenslToState[S, T, A, B](lens: Lens[S, T, A, B]): State[S, A] =
    lens.toState

  implicit def toStateLensOps[S, T, A, B](lens: Lens[S, T, A, B]): StateLensOps[S, T, A, B] =
    new StateLensOps[S, T, A, B](lens)

  implicit def toSimpleStateLensOps[S, A](lens: SimpleLens[S, A]): StateSimpleLensOps[S, A] =
    new StateSimpleLensOps[S, A](lens)

  def zoom[S, A, B](lens: SimpleLens[S, A])(subState: State[A, B]): State[S, B] =
    for {
      _ <- lens.toState
      b <- State[S, B] { s =>
        val (a, b) = subState.apply(lens.get(s))
        (lens.set(s, a), b)
      }
    } yield b
}

final class StateLensOps[S, T, A, B](lens: Lens[S, T, A, B]){
  def toState: State[S, A] = State(s => (s, lens.get(s)))

  def updateState(f: A => B): IndexedState[S, T, A] =
    toState.leftMap( lens.modify(_, f) )

  def ~=(f: A => B): IndexedState[S, T, A] = updateState(f)

  def assign(value: B): IndexedState[S, T, A] =
    updateState(_ => value)

  def :=(value: B): IndexedState[S, T, A] = assign(value)
}

final class StateSimpleLensOps[S, A](lens: SimpleLens[S, A]) {
  private val lensOps = new StateLensOps(lens)

  def +=(value: A)(implicit ev: Numeric[A]): State[S, A] =
    lensOps.updateState(a => ev.plus(a, value))

  def -=(value: A)(implicit ev: Numeric[A]): State[S, A] =
    lensOps.updateState(a => ev.minus(a, value))

  def *=(value: A)(implicit ev: Numeric[A]): State[S, A] =
    lensOps.updateState(a => ev.times(a, value))

}