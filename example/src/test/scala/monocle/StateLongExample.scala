package monocle

import org.specs2.scalaz.Spec
import monocle._, Monocle._
import monocle.state.lens._
import monocle.state.optional._

import scalaz.State
import scalaz.IndexedState._


class StateLongExample extends Spec {

  case class Game(deck: List[Card], player1: Player, player2: Player)

  case class Player(name: String, score: Int, hand: List[Card])

  case class Card(rank: Rank, suit: Suit)

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

  sealed trait Suit
  case object Hearths  extends Suit
  case object Diamonds extends Suit
  case object Spades   extends Suit
  case object Clubs    extends Suit

  val take1Card: State[List[Card], Option[Card]] = for {
    currentCards   <- get
    firstCard      <- headOption[List[Card], Card]
    remainingCards <- tailOption[List[Card], List[Card]]
    _              <- put(remainingCards getOrElse currentCards)
  } yield firstCard

  val deal1Card: State[Game, Option[Unit]] = for {
    firstCard <- zoom()
  }


}
