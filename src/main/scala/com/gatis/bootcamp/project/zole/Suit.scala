package com.gatis.bootcamp.project.zole

import cats.syntax.either._

sealed abstract class Suit private (val character: Char, val name: String) {
  import Suit._
  // override def toString: String = character.toString
  override def toString: String = this match {
    case Clubs    => "♣"
    case Spades   => "♠"
    case Hearts   => "♥"
    case Diamonds => "♦"
  }
  def queenAndJackSuitStrength = Suit.ordered.indexOf(this)
}

object Suit {
  case object Diamonds extends Suit('d', "diamonds")
  case object Hearts extends Suit('h', "hearts")
  case object Spades extends Suit('s', "spades")
  case object Clubs extends Suit('c', "clubs")

  val ordered: List[Suit] = Diamonds :: Hearts :: Spades :: Clubs :: Nil

  def of(x: Char): Either[ErrorMessage, Suit] = x match {
    case Clubs.character    => Clubs.asRight
    case Spades.character   => Spades.asRight
    case Hearts.character   => Hearts.asRight
    case Diamonds.character => Diamonds.asRight
    case _                  => s"Invalid char for suit: $x".asLeft
  }
}
