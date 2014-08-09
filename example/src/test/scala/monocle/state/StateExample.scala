package monocle.state

import monocle.state.lens._
import monocle.{Macro, SimpleLens}
import org.specs2.scalaz.Spec

import scalaz.State


class StateExample extends Spec {

  "assign, increment, decrement and multiply" in {
    case class Point(x: Int, y: Int)
    val _x = SimpleLens[Point](_.x)((p, x) => p.copy(x = x))
    val _y = SimpleLens[Point](_.y)((p, y) => p.copy(y = y))

    val variousUpdate: State[Point, Unit] = for {
      _ <- _x := 3
      _ <- _x += 2
      _ <- _y -= 2
      _ <- _y *= 5
    } yield ()

    variousUpdate.exec(Point(0, 0)) shouldEqual Point(5, -10)

  }

  "zoom" in {
    case class Game(deck1: List[Int], deck2: List[Int])
    val _deck1 = Macro.mkLens[Game, List[Int]]("deck1")

    val take1Card: State[List[Int], Option[Int]] =
      State[List[Int], Option[Int]]{
        case Nil     => (Nil, None)
        case x :: xs => (xs, Some(x))
      }

    val liftTake1Card: State[Game, Option[Int]] = zoom(_deck1)(take1Card)

    val game = Game(List(1,2,3), List(4))
    liftTake1Card.run(game) shouldEqual (Game(List(2,3), List(4)), Some(1))
  }

}
