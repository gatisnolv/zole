package com.gatis.bootcamp.project.zole

sealed abstract class Rank private (val value: Char, val points: Int) {
  override def toString: String = value.toString
  def strength: Int = Rank.ordered.indexOf(this)
}

object Rank {
  case object Seven extends Rank('7', 0)
  case object Eight extends Rank('8', 0)
  case object Nine extends Rank('9', 0)
  case object Ten extends Rank('T', 10)
  case object Jack extends Rank('J', 2)
  case object Queen extends Rank('Q', 3)
  case object King extends Rank('K', 4)
  case object Ace extends Rank('A', 11)

  val ordered: List[Rank] = Seven :: Eight :: Nine :: King :: Ten :: Ace :: Jack :: Queen :: Nil

  def of(x: Char): Either[ErrorMessage, Rank] = {
    ordered.find(_.value == x).toRight(s"Unrecognized rank $x")
  }
}
