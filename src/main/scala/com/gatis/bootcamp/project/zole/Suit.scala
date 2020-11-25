package com.gatis.bootcamp.project.zole

import cats.syntax.either._

sealed abstract class Suit private (val character: Char) {
  override def toString: String = character.toString
}

object Suit {
  case object Hearts extends Suit('h')
  case object Diamonds extends Suit('d')
  case object Clubs extends Suit('c')
  case object Spades extends Suit('s')

  //not sure if this will be needed
  def of(x: Char): Either[ErrorMessage, Suit] = x match {
    case Hearts.character   => Hearts.asRight
    case Diamonds.character => Diamonds.asRight
    case Clubs.character    => Clubs.asRight
    case Spades.character   => Spades.asRight
    case _                  => s"Invalid char for suit: $x".asLeft
  }
}
