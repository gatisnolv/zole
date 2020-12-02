package com.gatis.bootcamp.project.zole

sealed abstract class Rank private (val value: Char) {
  override def toString: String = value.toString
  // applies to trump cards, for full comparison between cards, need to take suit into account
  def strength: Int = Rank.ordered.indexOf(this)
}

object Rank {
  case object Seven extends Rank('7')
  case object Eight extends Rank('8')
  case object Nine extends Rank('9')
  case object Ten extends Rank('T')
  case object Jack extends Rank('J')
  case object Queen extends Rank('Q')
  case object King extends Rank('K')
  case object Ace extends Rank('A')

  val ordered: List[Rank] = Seven :: Eight :: Nine :: King :: Ten :: Ace :: Jack :: Queen :: Nil

  //not sure if this will be needed
  def of(x: Char): Either[ErrorMessage, Rank] = {
    //minor detail: maybe change to map?
    ordered.find(_.value == x).toRight(s"Unrecognized rank $x")
  }
}
