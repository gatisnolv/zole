package com.gatis.bootcamp.project.zole

import com.gatis.bootcamp.project.zole.GameChoice._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._

sealed abstract class ResultCategory(soloScore: Int, opponentScore: Int) {
  def score(isSolo: Boolean) = if (isSolo) soloScore else opponentScore
}

object ScoreProvider {
  case object ZoleZeroTricks extends ResultCategory(-16, 8)
  case object ZoleUnder31 extends ResultCategory(-14, 7)
  case object Zole31To60 extends ResultCategory(-12, 6)
  case object Zole61To90 extends ResultCategory(10, -5)
  case object ZoleAbove90 extends ResultCategory(12, -6)
  case object ZoleAllTricks extends ResultCategory(14, -7)

  case object BigZeroTricks extends ResultCategory(-8, 4)
  case object BigUnder31 extends ResultCategory(-6, 3)
  case object Big31To60 extends ResultCategory(-4, 2)
  case object Big61To90 extends ResultCategory(2, -1)
  case object BigAbove90 extends ResultCategory(4, -2)
  case object BigAllTricks extends ResultCategory(6, 3)

  case object SmallZoleWon extends ResultCategory(12, -6)
  case object SmallZoleLost extends ResultCategory(-14, 7)

  case object TheTableCategory extends ResultCategory(-4, 2)

  private def missingSolo(gameType: GameType) =
    s"Unexpected error: missing solo with game type $gameType".asLeft[Int]

  private def points(
    playedAlone: Boolean,
    player: Player,
    tricks: List[Trick]
  ): Either[ErrorMessage, Int] = {

    def pointsFromTricksTaken(player: Player) = tricks.foldLeft(0.asRight[ErrorMessage])(
      (acc, trick) =>
        for {
          taker <- trick.taker
          counted <- acc
        } yield if (taker == player) counted + trick.points else counted
    )

    if (playedAlone) pointsFromTricksTaken(player)
    else pointsFromTricksTaken(player).map(120 - _)
  }

  private def trickCounts(tricks: List[Trick]) = tricks.foldLeft(
    Map.empty[Player, Int].asRight[ErrorMessage]
  )((acc, trick) =>
    for {
      taker <- trick.taker
      counts <- acc
    } yield counts.updatedWith(taker)(count => count.fold(0)(_ + 1).some)
  )

  // isSolo doubles also for the losing player in TheTable case for convenience of this mapping
  // (there is no actual 'solo' player in the game type)
  def score(
    player: Player,
    gameType: GameType,
    tricks: List[Trick],
    playsSolo: Option[Player]
  ): Either[ErrorMessage, Int] = {

    def trickCount(player: Player) = trickCounts(tricks).map(_.getOrElse(player, 0))

    def isSolo = playsSolo.contains(player)

    def getLoser = {

      val maxTrickCount = trickCounts(tricks).map(_.toList.map({ case (_, count) => count }).max)
      for {
        max <- maxTrickCount
        counts <- trickCounts(tricks)
        pointsOfPlayersWithMostTricks <- counts.toList
          .filter({ case (_, count) => count == max })
          .map({ case (player, _) => points(true, player, tricks).map((player, _)) })
          .sequence
      } yield pointsOfPlayersWithMostTricks.maxBy { case (_, score) => score }

    }

    gameType match {
      case TheTable => scoreTheTable(player, tricks)
      case _ =>
        playsSolo.fold(missingSolo(gameType))(solo =>
          for {
            tricksTaken <- trickCount(solo)
            points <- points(isSolo, solo, tricks)
            score <- gameType match {
              case SmallZole => scoreSmallZole(isSolo, tricksTaken).asRight
              case _ => {
                val isBig = gameType == Big
                if (isBig || gameType == Zole)
                  scoreBigOrZole(isBig, points, isSolo, tricksTaken).asRight
                else s"Unexpected error: $gameType scored incorrectly".asLeft
              }
            }
          } yield score
        )
    }
  }

  private def scoreBigOrZole(isBig: Boolean, points: Int, isSolo: Boolean, tricksWon: Int) =
    tricksWon match {
      case 0 =>
        if (isBig) BigZeroTricks.score(isSolo) else ZoleZeroTricks.score(isSolo)
      case 8 =>
        if (isBig) BigAllTricks.score(isSolo) else ZoleAllTricks.score(isSolo)
      case _ =>
        if (points <= 30)
          if (isBig) BigUnder31.score(isSolo) else ZoleUnder31.score(isSolo)
        else if (points > 30 && points <= 60)
          if (isBig) Big31To60.score(isSolo) else Zole31To60.score(isSolo)
        else if (points > 60 && points <= 90)
          if (isBig) Big61To90.score(isSolo) else Zole61To90.score(isSolo)
        else { if (isBig) BigAbove90.score(isSolo) else ZoleAbove90.score(isSolo) }
    }

  private def scoreTheTable(player: Player, tricks: List[Trick]) = {
    def getLoser = {
      val maxTrickCount = trickCounts(tricks).map(_.toList.map({ case (_, count) => count }).max)
      for {
        max <- maxTrickCount
        counts <- trickCounts(tricks)
        pointsOfPlayersWithMostTricks <- counts.toList
          .filter({ case (_, count) => count == max })
          .map({ case (player, _) => points(true, player, tricks).map((player, _)) })
          .sequence
      } yield pointsOfPlayersWithMostTricks.maxBy { case (_, score) => score } match {
        case (player, _) => player
      }

    }

    getLoser.map(loser => TheTableCategory.score(player == loser))
  }

  private def scoreSmallZole(isSolo: Boolean, tricksWon: Int) =
    if (tricksWon == 0) SmallZoleWon.score(isSolo) else SmallZoleLost.score(isSolo)

}
