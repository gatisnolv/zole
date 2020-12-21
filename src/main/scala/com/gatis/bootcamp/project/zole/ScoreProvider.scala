package com.gatis.bootcamp.project.zole

import com.gatis.bootcamp.project.zole.GameChoice._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._

sealed trait ResultCategory

sealed abstract class ZoleOrBigCategory(
  zoleSoloScore: Int,
  zoleOpponentScore: Int,
  bigSoloScore: Int,
  bigOpponentScore: Int
) extends ResultCategory {
  def score(isSolo: Boolean, isBig: Boolean) =
    if (isBig) if (isSolo) bigSoloScore else bigOpponentScore
    else (
      if (isSolo) zoleSoloScore
      else zoleOpponentScore
    )
}

sealed abstract class SmallZoleOrTableCategory(soloScore: Int, opponentScore: Int)
    extends ResultCategory {
  def score(isSolo: Boolean) = if (isSolo) soloScore else opponentScore
}

object ScoreProvider {
  case object ZeroTricks extends ZoleOrBigCategory(-16, 8, -8, 4)
  case object PointsUnder31 extends ZoleOrBigCategory(-14, 7, -6, 3)
  case object Points31To60 extends ZoleOrBigCategory(-12, 6, -4, 2)
  case object Points61To90 extends ZoleOrBigCategory(10, -5, 2, -1)
  case object PointsAbove90 extends ZoleOrBigCategory(12, -6, 4, -2)
  case object AllTricks extends ZoleOrBigCategory(14, -7, 6, -3)

  case object SmallZoleWon extends SmallZoleOrTableCategory(12, -6)
  case object SmallZoleLost extends SmallZoleOrTableCategory(-14, 7)

  // soloScore here is that of the losing player for consistency of the overall score mapping
  // (there is no actual 'solo' player in the game type TheTable)
  case object TheTableCategory extends SmallZoleOrTableCategory(-4, 2)

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

  def score(
    player: Player,
    gameType: GameType,
    tricks: List[Trick],
    playsSolo: Option[Player]
  ): Either[ErrorMessage, Int] = {

    def trickCount(player: Player) = trickCounts(tricks).map(_.getOrElse(player, 0))

    def isSolo = playsSolo.contains(player)

    def missingSolo(gameType: GameType) =
      s"Unexpected error: missing solo with game type $gameType".asLeft[Int]

    if (gameType == TheTable) scoreTheTable(player, tricks)
    else
      playsSolo.fold(missingSolo(gameType))(solo =>
        for {
          tricksTaken <- trickCount(solo)
          points <- points(isSolo, solo, tricks)
          score <-
            if (gameType == SmallZole) scoreSmallZole(isSolo, tricksTaken).asRight
            else scoreBigOrZole(gameType, points, isSolo, tricksTaken)
        } yield score
      )
  }

  private def scoreBigOrZole(gameType: GameType, points: Int, isSolo: Boolean, tricksWon: Int) = {
    val isBig = gameType == Big
    if (isBig || gameType == Zole)
      (tricksWon match {
        case 0 =>
          ZeroTricks.score(isSolo, isBig)
        case 8 =>
          AllTricks.score(isSolo, isBig)
        case _ =>
          if (points <= 30)
            PointsUnder31.score(isSolo, isBig)
          else if (points > 30 && points <= 60)
            Points31To60.score(isSolo, isBig)
          else if (points > 60 && points <= 90)
            Points61To90.score(isSolo, isBig)
          else PointsAbove90.score(isSolo, isBig)
      }).asRight
    else s"Unexpected error: $gameType should be scored with different method".asLeft

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
