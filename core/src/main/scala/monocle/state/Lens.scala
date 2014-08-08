package monocle.state

import monocle.{Lens, SimpleLens}

import scalaz._


object lens {
  implicit def toStateLensOps[S, T, A, B](lens: Lens[S, T, A, B]): StateLensOps[S, T, A, B] =
    new StateLensOps[S, T, A, B](lens)

  implicit def toSimpleStateLensOps[S, A](lens: SimpleLens[S, A]): StateSimpleLensOps[S, A] =
    new StateSimpleLensOps[S, A](lens)
}

final class StateLensOps[S, T, A, B](lens: Lens[S, T, A, B]){
  def toState: State[S, A] = State(s => (s, lens.get(s)))

  def updateState(f: A => B): IndexedState[S, T, A] =
    toState.leftMap( lens.modify(_, f) )

  def assign(value: B): IndexedState[S, T, A] =
    updateState(_ => value)
}

final class StateSimpleLensOps[S, A](lens: SimpleLens[S, A]) {
  private val lensOps = new StateLensOps(lens)

  def +=(value: A)(implicit ev: Numeric[A]): State[S, A] =
    lensOps.updateState(a => ev.plus(a, value))

  def -=(value: A)(implicit ev: Numeric[A]): State[S, A] =
    lensOps.updateState(a => ev.minus(a, value))

  def *=(value: A)(implicit ev: Numeric[A]): State[S, A] =
    lensOps.updateState(a => ev.times(a, value))

  def zoom[B](subState: State[A, B]): State[S, B] =
    for {
      _ <- lensOps.toState
      b <- State[S, B] { s =>
        val (a, b) = subState.apply(lens.get(s))
        (lens.set(s, a), b)
      }
    } yield b
}