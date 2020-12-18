package com.gatis.bootcamp.project.zole

import cats.syntax.either._

//stiķis
case class Trick private (cards: List[Card]) {
  private def containsTrump = cards.exists(_.isTrump)
  def points = cards.foldLeft(0)((acc, card) => acc + card.points)

  def decideWinner = {
    val incomplete = "Winners can be decided only for complete tricks.".asLeft[Card]
    cards.lastOption.fold(incomplete)(first =>
      if (isComplete)
        if (containsTrump) cards.max.asRight
        //for non-trump cards, the round is decided by the strongest card of demanded suit, so order here is important
        else cards.filter(_.suit == first.suit).max.asRight
      else incomplete
    )
  }

  def isComplete = cards.length >= 3

  def add(card: Card) =
    if (isComplete) "This trick is already complete and has 3 cards, can't add more".asLeft
    else copy(cards = card :: cards).asRight
}

object Trick {
  def start(card: Card) = Trick(card :: Nil)
}
