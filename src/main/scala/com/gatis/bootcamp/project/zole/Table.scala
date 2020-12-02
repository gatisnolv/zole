package com.gatis.bootcamp.project.zole

import scala.util.Random

case class StashedCards(card1: Card, card2: Card)

// extends App for quick testing
object Table extends App {
  //write a test for this that it returns a list of 8,8,8,2 sets of cards
  def dealCards = {
    val deal = Random.shuffle(allCards).sliding(8, 8).toList.map(_.toSet)
    //store deal for game
    deal
  }

  def arrangeCardsInHand(cards: Set[Card]) = cards.toList.sorted(Card.InHandPrettyOrdering).reverse

  //norakt kārtis
  def stashCards(cards: StashedCards) = {
    // store stashed cards for player
    ???
  }

  def allCards = {
    val eithers = for {
      suit <- Suit.ordered
      rank <- Rank.ordered
      // } yield Card.of(rank.toString + suit.toString)
      // for easier visual checking, when suits toString defined as emoji
    } yield Card.of(rank.toString + suit.character.toString)

    eithers.foldLeft(Nil: List[Card])((acc, el) =>
      el match {
        case Right(card) => card :: acc
        case Left(_)     => acc
      }
    )
  }

  println(allCards.sorted.reverse)
  println(arrangeCardsInHand(allCards.toSet))
  println(dealCards.toList map arrangeCardsInHand)
  println(dealCards.toList map arrangeCardsInHand)

}
