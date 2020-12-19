package com.gatis.bootcamp.project.zole

import cats.syntax.either._
import cats.syntax.option._

// stiķis
case class Trick private (cards: List[Card], winner: Option[Player]) {
  private def containsTrump = cards.exists(_.isTrump)
  def points = cards.foldLeft(0)((acc, card) => acc + card.points)

  def winningCard = {
    val incomplete = "Winning cards can be decided only for complete tricks.".asLeft[Card]
    cards.lastOption.fold(incomplete)(first =>
      if (isComplete)
        if (containsTrump) cards.max.asRight
        // for non-trump cards, the round is decided by the strongest card of demanded suit, so order here is important
        else cards.filter(_.suit == first.suit).max.asRight
      else incomplete
    )
  }

  def isComplete = cards.length >= 3

  def add(card: Card) =
    if (isComplete) "This trick is already complete and has 3 cards, can't add more".asLeft
    else copy(cards = card :: cards).asRight

  def setWinner(player: Player) =
    if (isComplete) copy(winner = player.some).asRight
    else "A winner can be set only for complete trickes".asLeft
}

object Trick {
  def start(card: Card) = Trick(card :: Nil, None)
}
