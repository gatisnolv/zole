package com.gatis.bootcamp.project.zole

import scala.util.Random
import com.gatis.bootcamp.project.zole.Card._
import cats.syntax.either._
import cats.syntax.option._
import com.gatis.bootcamp.project.zole.GameChoice._
import cats.syntax.traverse._

case class Table private (val players: List[Player], val round: Option[Round]) {

  def hasPlayerNamed(name: String) = players.exists(_.name == name)

  def playerWithId(id: String): Either[ErrorMessage, Player] =
    players.find(_.id == id).toRight(s"Player with id $id is not sitting at this table.")

  def playersNeeded = 3 - players.length

  def morePlayersNeeded = playersNeeded > 0

  def getRound: Either[ErrorMessage, Round] =
    round.toRight(s"Need $playersNeeded more players to start playing.")

  def nextAfter(player: Player) =
    getRound.map(_ => players((players.indexOf(player) + 1) % players.length))

  def getGame = getRound.map(_.game)

  def passedOrdered = getRound.map(_.passed.reverse)

  def getTableCards = getRound.map(_.tableCards)

  def whoPlayedWhatInLastTrickOrdered = getRound.map(_.lastTrick)

  def hand(id: String): Either[ErrorMessage, List[Card]] = for {
    round <- getRound
    player <- playerWithId(id)
    hand <- round.hand(player)
  } yield Table.arrangeCardsInHand(hand)

  def turnToMakeGameChoice = for {
    round <- getRound
    game <- getGame
    result <- game.fold(round.makesGameChoice.asRight[ErrorMessage])(_ =>
      s"Game type has already been determined by ${round.makesGameChoice}".asLeft
    )
  } yield result

  def gameChoiceExpected = turnToMakeGameChoice.isRight

  def soloPlayer = for {
    round <- getRound
    game <- getGame
    solo <- game.fold(
      "Game type has not been determined, thus if and who will play solo.".asLeft[Player]
    )(gameType =>
      round.playsSolo.fold(s"The game type $gameType does not have a solo player.".asLeft[Player])(
        player => player.asRight
      )
    )
  } yield solo

  def roundHasSoloPlayer = soloPlayer.isRight

  def soloPlayersOpponents = soloPlayer.map(solo => players.filterNot(_ == solo))

  def soloIfNeedsToStash = for {
    game <- getGame
    tableCards <- getTableCards
    result <- game.fold(
      s"Game type has not been determined, cards are not needed to be stashed now.".asLeft[Player]
    )(gameType =>
      if (gameType != Big)
        s"Cards only need to be stashed, when the game type is Big, but it is $gameType.".asLeft
      else
        soloPlayer.flatMap(solo =>
          if (tableCards.stashed) s"Cards have already been stashed by $solo".asLeft
          else solo.asRight
        )
    )

  } yield result

  def soloNeedsToStash = soloIfNeedsToStash.isRight

  def turnToPlayCard = for {
    round <- getRound
    result <-
      if (roundIsComplete) "The round is complete, an new round can be started".asLeft
      else if (gameChoiceExpected)
        turnToMakeGameChoice.flatMap(makesChoice =>
          s"$makesChoice needs to make a game choice before anybody can play a card.".asLeft
        )
      else if (soloNeedsToStash)(
        soloIfNeedsToStash
          .flatMap(solo => s"$solo needs to stash cards before anybody can play a card.".asLeft)
        )
      else round.turn.asRight
  } yield result

  def cardCanBePlayed = turnToPlayCard.isRight && !roundIsComplete

  def roundIsComplete = round.fold(false)(_.isComplete == true)

  def seatPlayer(name: String, id: String): Either[ErrorMessage, Table] =
    if (morePlayersNeeded)
      if (hasPlayerNamed(name))
        s"There is already someone at the table with name $name, please use a different name.".asLeft
      else {
        val newTable = copy(players = players :+ Player.of(name, id))
        if (newTable.morePlayersNeeded) newTable.asRight else newTable.firstRound
      }
    else "There are 3 players at this table already.".asLeft

  def nextRound(id: String) = for {
    _ <- playerWithId(id)
    round <- getRound
    next <-
      if (round.isComplete) for {
        next <- nextAfter(round.firstHand)
        scoredPlayers <- players
          .map(player => round.score(player).flatMap(score => player.updateScore(score).asRight))
          .sequence
        table <- newRound(next, scoredPlayers)
      } yield table
      else "Next round can be started only when the current is finished.".asLeft
  } yield next

  private def newRound(first: Player, players: List[Player]): Either[ErrorMessage, Table] =
    if (players.length < 3)
      s"3 players needed to play, there are now only ${players.length}.".asLeft
    else {
      val deal = Table.dealCards
      if (
        deal.nonEmpty && deal.length == 4 && deal.init.forall(_.size == 8) && deal.last.size == 2
      ) {
        val hands = (players zip deal.init).toMap
        Round
          .start(first, hands, deal.last)
          .map(round => copy(players = players, round = round.some))
      } else "Unexpected error: dealing cards".asLeft
    }

  def makeGameChoice(id: String, choice: String): Either[ErrorMessage, Table] = {

    def pickGameTypeOrPass(player: Player) = for {
      round <- getRound
      game <- GameChoice.getGameType(choice, round.passed.length)
      next <- nextAfter(player)
      newRound <- game.fold(round.pass(player, next).asRight[ErrorMessage])(gameType => {
        val roundWithGameType = round.setGameType(gameType)
        gameType match {
          case TheTable => roundWithGameType.asRight
          case gameType => {
            val roundWithSolo = roundWithGameType.setSolo(player)
            if (gameType == Big) roundWithSolo.takeTableCards(player) else roundWithSolo.asRight
          }
        }
      })
    } yield copy(round = newRound.some)

    doActionIfTurn(id, turnToMakeGameChoice, pickGameTypeOrPass, "make a game choice")
  }

  def stashCards(id: String, cards: String): Either[ErrorMessage, Table] = {

    def stash(player: Player) = for {
      round <- getRound
      cards <- Card.multiple(cards)
      tableCards <- TableCards.of(cards.toSet)
      newRound <- round.stashCards(player, tableCards)
    } yield copy(round = newRound.some)

    doActionIfTurn(id, soloIfNeedsToStash, stash, "stash cards")
  }

  def playCard(id: String, card: String): Either[ErrorMessage, Table] = {

    def play(player: Player): Either[ErrorMessage, Table] = for {
      round <- getRound
      card <- Card.of(card)
      next <- nextAfter(player)
      newRound <- round.playCard(player, next, card)
    } yield copy(round = newRound.some)

    doActionIfTurn(id, turnToPlayCard, play, "play a card")
  }

  private def doActionIfTurn(
    id: String,
    turnToAct: Either[ErrorMessage, Player],
    action: Player => Either[ErrorMessage, Table],
    message: String
  ) = for {
    player <- playerWithId(id)
    turn <- turnToAct
    table <-
      if (player == turn) action(player)
      else s"It's not your turn to $message, it's $turn's turn".asLeft
  } yield table

  private def firstRound = players match {
    case first :: _ :: _ :: Nil => newRound(first, players)
    case _                      => s"Need $playersNeeded more players to start playing.".asLeft
  }

  def turnOrderInfo = getRound.map(_ =>
    "Players turns are in the following order: " +
      players.mkString(" -> ") + players.headOption.fold("")(first => s" (-> $first)")
  )

  def gameChoiceInfo(gamePreChoice: Option[GameType]) = gamePreChoice
    .fold(
      getGame.map(game =>
        game.fold("You passed.")(gameType =>
          s"${if (gameType == TheTable) "You passed last. " else ""}The game type will be $gameType."
        )
      )
    )(_ => "The game type was already set.".asLeft[String])

  def scores(id: String) = for {
    _ <- playerWithId(id)
    _ <- getRound
  } yield players.map(player => s"${player.name}: ${player.score}").mkString(", ")

  private def getCurrentTrickInfo(id: String) = for {
    player <- playerWithId(id)
    prefix = if (roundIsComplete) "The round is complete." else ""
    info <- getGame.flatMap(game =>
      game.fold("The game type has not yet been determined.".asRight[String])(gameType =>
        for {
          cardsPlayed <- whoPlayedWhatInLastTrickOrdered
          turn <- turnToPlayCard
        } yield (cardsPlayed match {
          case Nil =>
            s"No cards have been played in this trick yet."
          case (_, first) :: _ =>
            prefix + cardsPlayed
              .map({ case (player, card) => s"$player played $card" })
              .mkString(", ")
        }) + s". The game type is $gameType."
      )
    )
  } yield info

  private def getSoloInfo(id: String) = for {
    player <- playerWithId(id)
    _ <- getGame
    info <-
      if (roundHasSoloPlayer)
        for {
          solo <- soloPlayer
          opponents <- soloPlayersOpponents
        } yield
          (if (player == solo) "Your are" else s"$solo is") +
            s" playing solo against ${opponents.map(_.name).mkString(" and ")}."
      else "This game type does not have a solo player.".asRight
  } yield info

  private def getActionInfo(id: String) =
    if (roundIsComplete) "The round is complete, a new round can be started.".asRight
    else
      for {
        player <- playerWithId(id)
        game <- getGame
        needsToAct <-
          if (gameChoiceExpected) turnToMakeGameChoice
          else if (soloNeedsToStash) soloIfNeedsToStash
          else turnToPlayCard
        playerNeedsToAct = player == needsToAct
        passed <- passedOrdered
      } yield s"It's ${if (playerNeedsToAct) "your" else s"$needsToAct's"} turn to " +
        game.fold("make a game choice" + (passed match {
          case _ :: _ => ". " + passed.mkString(" and ") + " already passed"
          case _      => ""
        }))(_ => if (soloNeedsToStash) "stash two cards" else "play a card") + "."

  def statusInfo(id: String) = {
    val status = for {
      game <- getGame
      actionInfo <- getActionInfo(id)
      info <- game.fold(actionInfo.asRight[ErrorMessage])(_ =>
        for {
          soloInfo <- getSoloInfo(id)
          currentTrickInfo <- getCurrentTrickInfo(id)
        } yield s"$actionInfo $currentTrickInfo $soloInfo"
      )
    } yield info

    getGame.fold(_.asRight, _ => status)
  }

}

object Table {

  def empty = Table(Nil, None)

  def dealCards = Random.shuffle(allCards.toList).sliding(8, 8).toList.map(_.toSet)

  def arrangeCardsInHand(cards: Set[Card]) = cards.toList.sorted(Card.InHandPrettyOrdering).reverse

}
