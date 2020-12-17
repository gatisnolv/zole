package com.gatis.bootcamp.project.zole

// import cats.implicits._
import cats.syntax.either._
import cats.syntax.traverse._
import com.gatis.bootcamp.project.zole.Suit.Diamonds
import com.gatis.bootcamp.project.zole.Rank._

final case class Card(rank: Rank, suit: Suit) extends Ordered[Card] {
  import Card._

  override def compare(that: Card): Int =
    if (this.isTrump) {
      compareTrumpToOtherCard(this, that)
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
  val points = rank.points
}

object Card {

  private def compareTrumpToOtherCard(trump: Card, other: Card) =
    if (other.isTrump) // both trump
      if (trump.rank == other.rank) { // both either queens or jacks, so suit ordering determines strength
        trump.suit.queenAndJackSuitStrength - other.suit.queenAndJackSuitStrength
      } else trump.rank.strength - other.rank.strength
    else // other is non-trump
      1

  //only for pretty ordering in hand, keeping same suit cards together
  object InHandPrettyOrdering extends Ordering[Card] {
    override def compare(one: Card, other: Card): Int =
      if (one.isTrump) {
        compareTrumpToOtherCard(one, other)
      } else {
        if (other.isTrump) // this is non-trump, other trump
          -1
        else { // both non-trump
          if (one.suit == other.suit) { // non-trump cards of same rank don't have strength ordering
            one.rank.strength - other.rank.strength
          } else one.suit.queenAndJackSuitStrength - other.suit.queenAndJackSuitStrength
        }
      }
  }

  def allCards = {
    val eithers = for {
      suit <- Suit.ordered
      rank <- Rank.ordered
      // } yield Card.of(rank.toString + suit.toString)
      // for easier visual checking, when suits toString defined as emoji
    } yield Card.of(rank.toString + suit.character.toString)

    eithers.foldLeft(Set.empty[Card])((acc, el) =>
      el match {
        case Right(card) => acc + card
        case Left(_)     => acc
      }
    )
  }

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

  def multiple(x: String): Either[ErrorMessage, List[Card]] =
    x.trim.split("\\s*,\\s*").toList.map(of(_)).sequence
}
