package monocle.state

import scalaz.Order
import scalaz.syntax.semigroup._

case class Card(rank: Rank, suit: Suit)

object Card {
  implicit val order = Order.orderBy[Card, Rank](_.rank) |+| Order.orderBy[Card, Suit](_.suit)
}
