package com.gatis.bootcamp.project.zole

import scala.util.Random
import com.gatis.bootcamp.project.zole.Rank._
import com.gatis.bootcamp.project.zole.Suit._
import com.gatis.bootcamp.project.zole.Card._
import cats.syntax.either._
import cats.syntax.option._
import com.gatis.bootcamp.project.zole.GameChoice._

case class Table private (val players: List[Player], val round: Option[Round]) {

  def hasPlayerNamed(name: String) = players.exists(_.name == name)

  def playerWithId(id: String) = players
    .find(_.id == id)
    .fold(s"Player with id $id is not sitting at this table.".asLeft[Player])(_.asRight)

  def playersNeeded = 3 - players.length

  def morePlayersNeeded = playersNeeded > 0

  def getRound =
    round.fold(s"Need $playersNeeded more players to start playing.".asLeft[Round])(_.asRight)

  def nextAfter(player: Player) = getRound.map(_ => players((players.indexOf(player) + 1) % 3))

  def getGame = getRound.map(_.game)

  def playersPassed = getRound.map(_.playersPassed)

  def getTableCards = getRound.map(_.tableCards)

  def firstCardOfCurrentTrick = getRound.map(_.firstCardOfCurrentTrick)

  def whoPlayedWhatInCurrentTrickInOrder = getRound.flatMap(_.whoPlayedWhatInCurrentTrickInOrder)

  def playersCards(id: String): Either[ErrorMessage, List[Card]] = for {
    round <- getRound
    player <- playerWithId(id)
    cards <- round.playersCards(player)
  } yield Table.arrangeCardsInHand(cards)

  def whoseTurn = for {
    round <- getRound
    game <- getGame
    result <- game match {
      case None =>
        whoseTurnToMakeGameChoice.flatMap(makesChoice =>
          s"$makesChoice needs to make a game choice before anybody can play a card.".asLeft
        )
      case Some(value) => round.turn.asRight
    }
  } yield result
  getRound.map(_.turn)

  def cardsCanBePlayed = whoseTurn.isRight

  // different from turnToMakeGameChoice in that doesn't error when gameType is set, not sure if I'll need this yet
  def whoDeterminedGameType = ???

  def whoseTurnToMakeGameChoice = for {
    round <- getRound
    game <- getGame
    result <- game match {
      case None => round.makesGameChoice.asRight
      case Some(value) =>
        s"Game type has already been determined by ${round.makesGameChoice}".asLeft
    }
  } yield result

  def roundHasSoloPlayer = soloPlayer.isRight

  def soloNeedsToStash: Boolean = soloIfNeedsToStash.isRight

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

  def soloPlayer: Either[ErrorMessage, Player] = for {
    round <- getRound
    game <- getGame
    solo <- game match {
      case None =>
        "Game type has not been determined, it is yet to be determined if and who will play solo.".asLeft
      case Some(gameType) =>
        round.playsSolo match {
          case None         => s"The game type $gameType does not have a solo player.".asLeft
          case Some(player) => player.asRight
        }
    }
  } yield solo

  def soloPlayersOpponents = soloPlayer.map(solo => players.filterNot(_ == solo))

  def turnOrderInfo = getRound.map(_ =>
    "Players turns are in the following order: " + players
      .map(identity(_)) // for ease while developing - includes id
      .mkString(" -> ") + players.headOption.fold("")(first => s" (-> $first)")
  )

  def statusInfo(id: String) = {

    def getGameTypeInfo(game: Option[GameType]) = game.fold(
      "The game type has not yet been determined"
    )(gameType => s"The game type is $gameType") + ". "

    def getSoloInfo(player: Player) =
      if (roundHasSoloPlayer)
        for {
          solo <- soloPlayer
          opponents <- soloPlayersOpponents
        } yield
          (if (player == solo) "Your are" else s"$solo is") +
            s"playing solo against ${opponents.map(_.name).mkString(" and ")}. "
      else "This game type does not have a solo player. ".asRight

    def getActionInfo(playerNeedsToAct: Boolean, whoNeedsToAct: Player, game: Option[GameType]) =
      s"It's ${if (playerNeedsToAct) "your" else s"$whoNeedsToAct's"} turn to " +
        // TODO add info about who has already passed
        game.fold("make a game choice")(_ =>
          if (soloNeedsToStash) "stash two cards" else "play a card"
        ) + ". "

    def getCurrentTrickInfo(playerNeedsToAct: Boolean, playedCards: List[(Player, Card)]) = {
      val addressOfPlayer = if (playerNeedsToAct) "You" else "They"
      addressOfPlayer + (playedCards match {
        case Nil => " may play any card"
        case (_, card) :: _ =>
          s" must play a ${if (card.isTrump) "trump card"
          else s"card of ${card.suit.name} suit"} if ${addressOfPlayer.toLowerCase} have one," + playedCards
            .map({ case (player, card) => s"$player played $card" })
            .mkString(" and ")
      }) + ". "
    }

    def tableReadyInfo: Either[ErrorMessage, String] = for {
      player <- playerWithId(id)
      game <- getGame
      needsToAct <-
        game.fold(whoseTurnToMakeGameChoice)(_ => if (soloNeedsToStash) soloPlayer else whoseTurn)
      playerNeedsToAct = player == needsToAct
      basicInfo = getGameTypeInfo(game) + getActionInfo(playerNeedsToAct, needsToAct, game)
      info <- game.fold((basicInfo).asRight[ErrorMessage])(_ =>
        for {
          soloInfo <- getSoloInfo(player)
          playedCards <- whoPlayedWhatInCurrentTrickInOrder
        } yield basicInfo + soloInfo + getCurrentTrickInfo(playerNeedsToAct, playedCards)
      )
    } yield info

    // check whether this makes sense
    tableReadyInfo.fold(identity(_), identity(_))
  }

  def gameChoiceInfo(gamePreChoice: Option[GameType]): Either[ErrorMessage, String] = gamePreChoice
    .fold(
      getGame.map(game =>
        game.fold("You passed. ")(gameType =>
          if (gameType == TheTable) "You passed last. "
          else "" + s"The game type will be $gameType. "
        )
      )
    )(_ => "The game type was already set".asLeft[String])

  def seatPlayer(name: String, id: String): Either[ErrorMessage, Table] =
    if (morePlayersNeeded)
      if (hasPlayerNamed(name))
        s"There is already someone at the table with name $name, please use a different name.".asLeft
      else {
        // a choice to have the first seated be the starting player, so appending here
        val newTable = copy(players = players :+ Player.of(name, id))
        if (morePlayersNeeded) newTable.asRight else newTable.firstRound
      }
    else "There are 3 players at this table already.".asLeft

  def makeGameChoice(id: String, choice: String): Either[ErrorMessage, Table] = {

    def pickGameTypeOrPass(round: Round, player: Player) = for {
      gameType <- GameChoice.getGameType(choice, round.playersPassed)
      next <- nextAfter(player)
      newRound <- gameType.fold(round.pass(next).asRight[ErrorMessage])(gameType => {
        val roundWithGameType = round.setGameType(gameType)
        gameType match {
          case TheTable => roundWithGameType.asRight
          case Big      => roundWithGameType.takeTableCards(player)
          case _        => roundWithGameType.setSolo(player).asRight
        }
      })
      newTable <- copy(round = newRound.some).asRight
    } yield newTable

    for {
      player <- playerWithId(id)
      makesChoice <- whoseTurnToMakeGameChoice
      round <- getRound
      table <-
        if (makesChoice == player) pickGameTypeOrPass(round, player)
        else s"It's not your turn to make a game choice, it's $makesChoice's turn.".asLeft
    } yield table
  }

  def stashCards(id: String, cards: String): Either[ErrorMessage, Table] = {

    def getNewTable(round: Round, player: Player) = for {
      cards <- Card.multiple(cards)
      tableCards <- TableCards.of(cards.toSet)
      newRound <- round.stashCards(player, tableCards)
      table <- copy(round = newRound.some).asRight
    } yield table

    for {
      player <- playerWithId(id)
      round <- getRound
      game <- getGame
      solo <- soloIfNeedsToStash
      playerIsSolo = player == solo
      table <-
        if (playerIsSolo) getNewTable(round, player)
        else s"You don't need to stash, $solo does.".asLeft
    } yield table
  }

  //rework
  def playCard(id: String, card: String): Either[ErrorMessage, Table] = for {
    player <- playerWithId(id)
    turn <- whoseTurn
    game <- getGame
    makesChoice <- whoseTurnToMakeGameChoice
    table <-
      game.fold(
        s"Game type has not been determined, it's $makesChoice's turn to make a game choice."
          .asLeft[Table]
      )(gameType =>
        if (turn == player) {
          // continue here
          // add checks that a turn can actually happen, e.g. account for Big needing to stash two cards before the game can continue
          for {
            card <- Card.of(card)
          } yield ???
        } else
          s"It's not your turn to play a card, it's $turn's turn.".asLeft
      )
  } yield table

  def firstRound = newRound(players(0))

  // def nextRound = for {
  // round <- round
  // next <- next(round.firstHand)
  // newRound <- newRound(next)
  // } yield newRound

  private def newRound(first: Player): Either[ErrorMessage, Table] =
    if (players.length < 3)
      s"3 players needed to play, there are now only ${players.length}.".asLeft
    else {
      val deal = Table.dealCards
      //question is this 'sanity' check really needed here?
      if (deal.nonEmpty && deal.init.forall(_.size == 8)) {
        // q about use of init, last with regards to exception throwing, I guess, could wrap with Try
        val playersCards = (players zip deal.init).toMap
        Round.start(first, playersCards, deal.last).map(round => copy(round = round.some))
      }
      // This should be a 5xx
      else "Unexpected error: dealing cards".asLeft

    }
}

// extends App for quick testing
object Table extends App {
  //write a test for this that it returns a list of 8,8,8,2 sets of cards
  def dealCards = Random.shuffle(allCards.toList).sliding(8, 8).toList.map(_.toSet)

  def empty = Table(Nil, None)

  def arrangeCardsInHand(cards: Set[Card]) = cards.toList.sorted(Card.InHandPrettyOrdering).reverse

  println(allCards.toList.sorted.reverse)
  println(arrangeCardsInHand(allCards))
  println(dealCards.toList map arrangeCardsInHand)
  println(dealCards.toList map arrangeCardsInHand)
  println(s"total points add up to 120: ${allCards.foldLeft(0)((acc, card) => acc + card.points)}")

  def queen(suit: Suit) = Card(Queen, suit)
  def nineHearts = Card(Nine, Hearts)
  def tenHearts = Card(Ten, Hearts)
  def aceClubs = Card(Ace, Clubs)
  def sevenDiamonds = Card(Seven, Diamonds)

  println((Trick.of(queen(Clubs), queen(Diamonds), queen(Spades))).decideTrickWinner)
  println((Trick.of(queen(Spades), queen(Diamonds), queen(Clubs))).decideTrickWinner)
  println((Trick.of(nineHearts, tenHearts, aceClubs)).decideTrickWinner)
  println((Trick.of(nineHearts, tenHearts, sevenDiamonds)).decideTrickWinner)

  println(Trick.of(nineHearts, tenHearts, aceClubs).points)

  val testTable = Table(List(Player.of("a", "1"), Player.of("b", "2"), Player.of("c", "3")), None)
  val firstPlayer = testTable.players(0)
  println(testTable.nextAfter(firstPlayer))

}
