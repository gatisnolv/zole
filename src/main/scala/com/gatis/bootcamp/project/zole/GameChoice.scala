package com.gatis.bootcamp.project.zole

import cats.syntax.either._
import cats.syntax.option._

sealed abstract class GameChoice private (val shortName: String)

object GameChoice {
  trait GameType
  // lielais (pacelt galda kārtis)
  case object Big extends GameChoice("B") with GameType
  // zole
  case object Zole extends GameChoice("Z") with GameType
  // mazā zole
  case object SmallZole extends GameChoice("S") with GameType
  // garām
  case object Pass extends GameChoice("P")
  // galdiņš
  case object TheTable extends GameType

  def getGameType(choice: String, passes: Int): Either[ErrorMessage, Option[GameType]] =
    choice match {
      case Big.shortName       => Big.some.asRight
      case Zole.shortName      => Zole.some.asRight
      case SmallZole.shortName => SmallZole.some.asRight
      // 3 passes means galdiņš
      case Pass.shortName => (if (passes < 2) None else TheTable.some).asRight
      case _              => s"Invalid value for game choice: $choice".asLeft
    }
}
