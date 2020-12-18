package com.gatis.bootcamp.project.zole

//stiķis
case class Trick private (cards: List[Card]) {
  def toSet = cards.toSet
  def containsTrump = cards.exists(_.isTrump)
  def points = cards.foldLeft(0)((acc, card) => acc + card.points)

  def decideTrickWinner: Card = {
    if (containsTrump) cards.max
    //for non-trump cards, the round is decided by the strongest card of demanded suit, so order here is important
    else cards.filter(_.suit == cards.head.suit).max
  }
}

object Trick {
  def of(card1: Card, card2: Card, card3: Card) = Trick(card1 :: card2 :: card3 :: Nil)
}
