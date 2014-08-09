package monocle.state

import scalaz.Ordering.{LT, EQ, GT}
import scalaz.{Ordering, Order}

sealed trait Suit
case object Hearts   extends Suit
case object Diamonds extends Suit
case object Spades   extends Suit
case object Clubs    extends Suit

object Suit {
  private def intValue(suit: Suit): Int = suit match {
    case Hearts   => 4
    case Diamonds => 3
    case Spades   => 2
    case Clubs    => 1
  }

  implicit val order = new Order[Suit]{
    def order(x: Suit, y: Suit): Ordering = {
      val diff = intValue(x) - intValue(y)
      if(diff > 0)        GT
      else if (diff == 0) EQ
      else                LT
    }
  }
}
