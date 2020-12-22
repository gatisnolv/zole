package com.gatis.bootcamp.project.zole

import cats.syntax.either._

case class TableCards private (cards: Set[Card]) {
  def stashed = cards.nonEmpty
  def points = cards.map(_.points).sum
}

object TableCards {
  def of(cards: Set[Card]): Either[ErrorMessage, TableCards] =
    if (cards.size != 2) s"There should be exactly two table cards, got ${cards.size}".asLeft
    else TableCards(cards).asRight

  def empty = TableCards(Set.empty)
}
