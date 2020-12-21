package com.gatis.bootcamp.project.zole

import cats.syntax.either._
import cats.syntax.option._

// stiķis
case class Trick private (cardsPlayed: List[(Player, Card)]) {

  private def containsTrump = cardsPlayed.exists({ case (_, card) => card.isTrump })

  // acis
  def points = cardsPlayed.foldLeft(0)({ case (acc, (_, card)) => acc + card.points })

  def isComplete = cardsPlayed.length >= 3

  def play(card: Card, player: Player) =
    if (isComplete) "This trick is already has 3 cards and is complete, can't play more".asLeft
    else copy(cardsPlayed = (player, card) :: cardsPlayed).asRight

  // def whoPlayed(card: Card) = cardsPlayed
  //   .find { case (player, aCard) => card == aCard }
  //   .toRight("This card was not played in the current trick.")
  //   .map { case (player, _) => player }

  def taker = {
    implicit val completeTrickCardOrdering = new Ordering[(Player, Card)] {
      override def compare(one: (Player, Card), other: (Player, Card)): Int = (one, other) match {
        case ((_, oneCard), (_, otherCard)) => oneCard.compare(otherCard)
      }
    }
    val incomplete = "Winning cards can be decided only for complete tricks.".asLeft[Player]

    cardsPlayed.lastOption.fold(incomplete) { case (_, firstCard) =>
      if (isComplete)
        if (containsTrump)
          cardsPlayed max match { case (player, _) => player.asRight }
        // for non-trump cards, the winner is decided by the strongest card of demanded suit, so order here is important
        else
          cardsPlayed.filter { case (_, card) => card.suit == firstCard.suit } max match {
            case (player, _) => player.asRight
          }
      else incomplete
    }
  }

}

object Trick {
  def start(card: Card, player: Player) = Trick((player, card) :: Nil)
}
