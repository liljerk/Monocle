package monocle

import scalaz.std.anyVal._
import scalaz.syntax.std.boolean._
import scalaz.Maybe._
import scalaz.{Maybe, Foldable, Monoid, Tag, IList}

abstract class Fold[S, A] { self =>

  def foldMap[M: Monoid](f: A => M)(s: S): M

  @inline final def fold(s: S)(implicit ev: Monoid[A]): A = foldMap(identity)(s)

  @inline final def getAll(s: S): IList[A] = foldMap(IList(_))(s)
  @inline final def headMaybe(s: S): Maybe[A] = Tag.unwrap(foldMap(Maybe.just(_).first)(s))

  @inline final def exist(p: A => Boolean)(s: S): Boolean = Tag.unwrap(foldMap(p(_).disjunction)(s))
  @inline final def all(p: A => Boolean)(s: S): Boolean = Tag.unwrap(foldMap(p(_).conjunction)(s))


  final def composeFold[B](other: Fold[A, B]): Fold[S, B] = new Fold[S, B] {
    @inline def foldMap[M: Monoid](f: B => M)(s: S): M = self.foldMap(other.foldMap(f)(_))(s)
  }
  @inline final def composeGetter[C](other: Getter[A, C]): Fold[S, C] = composeFold(other.asFold)
  @inline final def composeTraversal[B, C, D](other: Traversal[A, B, C, D]): Fold[S, C] = composeFold(other.asFold)
  @inline final def composeOptional[B, C, D](other: Optional[A, B, C, D]): Fold[S, C] = composeFold(other.asFold)
  @inline final def composePrism[B, C, D](other: Prism[A, B, C, D]): Fold[S, C] = composeFold(other.asFold)
  @inline final def composeLens[B, C, D](other: Lens[A, B, C, D]): Fold[S, C] = composeFold(other.asFold)
  @inline final def composeIso[B, C, D](other: Iso[A, B, C, D]): Fold[S, C] = composeFold(other.asFold)
}

object Fold {

  def apply[F[_]: Foldable, A]: Fold[F[A], A] = new Fold[F[A], A] {
    @inline def foldMap[M: Monoid](f: A => M)(s: F[A]): M = Foldable[F].foldMap(s)(f)
  }

}
