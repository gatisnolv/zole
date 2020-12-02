package com.gatis.bootcamp.project.zole

object Table extends App {
  def dealCards = ???
  def stashCards = ???

  def allCards = {
    val eithers = for {
      suit <- Suit.ordered
      rank <- Rank.ordered
    } yield Card.of(rank.toString + suit.toString)

    eithers.foldLeft(Nil: List[Card])((acc, el) =>
      el match {
        case Right(card) => card :: acc
        case Left(_)     => acc
      }
    )
  }

  println(allCards.sorted)

}
