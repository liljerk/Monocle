package monocle.state

import monocle.{SimpleLens, Lenser}
import monocle.Monocle._
import monocle.state.lens._
import monocle.state.optional._
import org.specs2.scalaz.Spec

import scalaz.IndexedState._
import scalaz.State
import scalaz.syntax.order._


class StateLongExample extends Spec {

  case class Game(deck: List[Card], player1: Player, player2: Player)
  case class Player(name: String, score: Int, hand: List[Card])

  object Game {
    def initialise(name1: String, name2: String): Game =
      Game(Nil, Player.initialise(name1), Player.initialise(name2))

    private val lenser = Lenser[Game]
    val (_deck, _player1, _player2) = (lenser(_.deck), lenser(_.player1), lenser(_.player2))
  }

  object Player {
    def initialise(name: String): Player = Player(name, 0, Nil)

    private val lenser = Lenser[Player]
    val (_name, _score, _hand) = (lenser(_.name), lenser(_.score), lenser(_.hand))
  }

  import Game._, Player._

  val take1Card: State[List[Card], Option[Card]] = for {
    currentCards   <- get
    firstCard      <- headOption[List[Card], Card]
    remainingCards <- tailOption[List[Card], List[Card]]
    _              <- put(remainingCards getOrElse currentCards)
  } yield firstCard

  def deal1Card(card: Card): State[List[Card], Unit] = for {
    currentCards <- get
    _            <- put(card :: currentCards)
  } yield ()

  def deal1CardToPlayer(to: SimpleLens[Game, Player]): State[Game, Option[Unit]] = for {
    firstCard <- zoom(_deck)(take1Card)
    success   <- firstCard match {
      case None       => state[Game, Option[Unit]](None)
      case Some(card) => zoom(to composeLens _hand)(deal1Card(card)).map(Some(_))
    }
  } yield success

  def deal1CardToAllPlayers: State[Game, Option[Unit]] = for {
    _       <- deal1CardToPlayer(_player1)
    success <- deal1CardToPlayer(_player2)
  } yield success

  def increaseScore(to: SimpleLens[Game, Player]): State[Game, Unit] =
    zoom(to)(_score += 1).map(_ => ())

  def play1Card: State[Game, Option[Unit]] = for {
    player1Card <- zoom(_player1 composeLens _hand)(take1Card)
    player2Card <- zoom(_player2 composeLens _hand)(take1Card)
    success     <- (player1Card, player2Card) match {
      case (Some(c1), Some(c2)) =>
        (
          if     (c1 > c2) increaseScore(_player1)
          else if(c1 < c2) increaseScore(_player2)
          else increaseScore(_player1) flatMap (_ => increaseScore(_player2))
        ).map(Some(_))
      case _ => state[Game, Option[Unit]](None)
    }
  } yield success

  val newGame = Game.initialise("John", "Roger")

  "example" in {

    val action = for {
      _       <- deal1CardToAllPlayers
      _       <- deal1CardToAllPlayers
      success <- play1Card
    } yield success

    val cards = List(Card(Ten, Hearts), Card(Jack, Diamonds), Card(As, Clubs), Card(Eight, Clubs))

    action.run(_deck.set(newGame, cards)) shouldEqual (
      Game(Nil, Player("John",1,List(Card(Ten, Hearts))),Player("Roger",0,List(Card(Jack, Diamonds)))),
      Some(())
    )

  }

}
