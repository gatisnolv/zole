package com.gatis.bootcamp.project.zole

import cats.implicits._
import com.gatis.bootcamp.project.zole.Suit.Diamonds
import com.gatis.bootcamp.project.zole.Rank._

final case class Card(rank: Rank, suit: Suit) {
  override def toString: String = s"$rank$suit"
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
