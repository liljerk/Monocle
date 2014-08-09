package monocle.state

import scalaz.Ordering.{LT, EQ, GT}
import scalaz.{Ordering, Order}

sealed trait Rank
case object As     extends Rank
case object King   extends Rank
case object Queen  extends Rank
case object Jack   extends Rank
case object Ten    extends Rank
case object Nine   extends Rank
case object Eight  extends Rank
case object Seven  extends Rank
case object Six    extends Rank
case object Five   extends Rank
case object Fourth extends Rank
case object Three  extends Rank
case object Two    extends Rank
case object One    extends Rank

object Rank {

  private def intValue(rank: Rank): Int = rank match {
    case As     => 14
    case King   => 13
    case Queen  => 12
    case Jack   => 11
    case Ten    => 10
    case Nine   =>  9
    case Eight  =>  8
    case Seven  =>  7
    case Six    =>  6
    case Five   =>  5
    case Fourth =>  4
    case Three  =>  3
    case Two    =>  2
    case One    =>  1
  }

  implicit val order = new Order[Rank]{
    def order(x: Rank, y: Rank): Ordering = {
      val diff = intValue(x) - intValue(y)
      if(diff > 0)        GT
      else if (diff == 0) EQ
      else                LT
    }
  }
}