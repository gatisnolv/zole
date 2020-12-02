package com.gatis.bootcamp.project.zole

import cats.implicits._
import com.gatis.bootcamp.project.zole.Suit.Diamonds
import com.gatis.bootcamp.project.zole.Rank._

final case class Card(rank: Rank, suit: Suit) extends Ordered[Card] {

  override def compare(that: Card): Int =
    if (this.isTrump) {
      if (that.isTrump) // both trump
        if (this.rank == that.rank) { // both either queens or jacks, so suit ordering determines strength
          this.suit.queenAndJackSuitStrength - that.suit.queenAndJackSuitStrength
        } else this.rank.strength - that.rank.strength
      else // other is non-trump
        1
    } else {
      if (that.isTrump) // this is non-trump, other trump
        -1
      else { // both non-trump
        if (this.rank == that.rank) { // non-trump cards of same rank don't have strength ordering
          0
        } else this.rank.strength - that.rank.strength
      }
    }

  override def toString: String = s"$rank$suit"
  def isTrump = rank == Queen || rank == Jack || suit == Diamonds
}

object Card {
  def of(x: String): Either[ErrorMessage, Card] = x.toList match {
    case r :: s :: Nil =>
      for {
        rank <- Rank.of(r)
        suit <- Suit.of(s)
        result <-
          if (suit != Diamonds && (rank == Seven || rank == Eight))
            s"The card $rank$suit is not used in the game Zole".asLeft
          else Card(rank, suit).asRight
      } yield result
    case _ => s"Failed to parse card $x".asLeft
  }
}
