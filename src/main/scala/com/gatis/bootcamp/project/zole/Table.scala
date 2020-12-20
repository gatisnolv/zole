package com.gatis.bootcamp.project.zole

import scala.util.Random
import com.gatis.bootcamp.project.zole.Rank._
import com.gatis.bootcamp.project.zole.Suit._
import com.gatis.bootcamp.project.zole.Card._
import cats.syntax.either._
import cats.syntax.option._
import com.gatis.bootcamp.project.zole.GameChoice._
import cats.syntax.traverse._

case class Table private (val players: List[Player], val round: Option[Round]) {

  def hasPlayerNamed(name: String) = players.exists(_.name == name)

  def playerWithId(id: String) = players
    .find(_.id == id)
    .fold(s"Player with id $id is not sitting at this table.".asLeft[Player])(_.asRight)

  def playersNeeded = 3 - players.length

  def morePlayersNeeded = playersNeeded > 0

  def getRound =
    round.fold(s"Need $playersNeeded more players to start playing.".asLeft[Round])(_.asRight)

  def nextAfter(player: Player) =
    getRound.map(_ => players((players.indexOf(player) + 1) % players.length))

  def getGame = getRound.map(_.game)

  def passedOrdered = getRound.map(_.passed.reverse)

  def getTableCards = getRound.map(_.tableCards)

  def whoPlayedWhatInCurrentTrickInOrder = getRound.flatMap(_.whoPlayedWhatInCurrentTrickOrdered)

  def playersHand(id: String): Either[ErrorMessage, List[Card]] = for {
    round <- getRound
    player <- playerWithId(id)
    hand <- round.playersHand(player)
  } yield Table.arrangeCardsInHand(hand)

  // different from turnToMakeGameChoice in that doesn't error when gameType is set, not sure if I'll need this yet
  // def whoDeterminedGameType = ???

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
      if (gameChoiceExpected)
        turnToMakeGameChoice.flatMap(makesChoice =>
          s"$makesChoice needs to make a game choice before anybody can play a card.".asLeft
        )
      else if (soloNeedsToStash)(
        soloIfNeedsToStash
          .flatMap(solo => s"$solo needs to stash cards before anybody can play a card.".asLeft)
        )
      else round.turn.asRight
  } yield result

  def cardsCanBePlayed = turnToPlayCard.isRight

  def turnOrderInfo = getRound.map(_ =>
    "Players turns are in the following order: " + players
      // .map(_.name)
      // for ease while developing - includes id
      .map(identity(_))
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
          (if (player == solo) " Your are" else s"$solo is") +
            s"playing solo against ${opponents.map(_.name).mkString(" and ")}. "
      else " This game type does not have a solo player. ".asRight

    def getActionInfo(
      playerNeedsToAct: Boolean,
      whoNeedsToAct: Player,
      game: Option[GameType],
      passed: List[Player]
    ) = s"It's ${if (playerNeedsToAct) "your" else s"$whoNeedsToAct's"} turn to " +
      game.fold("make a game choice" + (passed match {
        case _ :: _ => ". " + passed.mkString(" and ") + " already passed"
        case _      => ""
      }))(_ => if (soloNeedsToStash) "stash two cards" else "play a card") + "."

    def getCurrentTrickInfo(playerNeedsToAct: Boolean, playedCards: List[(Player, Card)]) = {
      val addressOfPlayer = if (playerNeedsToAct) "You" else "They"
      addressOfPlayer + (playedCards match {
        case Nil => " may play any card"
        case (_, card) :: _ =>
          s" must play a ${if (card.isTrump) "trump card"
          else s"card of ${card.suit.name} suit"} if ${addressOfPlayer.toLowerCase} have one. " +
            playedCards
              .map({ case (player, card) => s"$player played $card" })
              .mkString(" and ")
      }) + ". "
    }

    def tableReadyInfo: Either[ErrorMessage, String] = for {
      player <- playerWithId(id)
      game <- getGame
      needsToAct <-
        if (gameChoiceExpected) turnToMakeGameChoice
        else if (soloNeedsToStash) soloIfNeedsToStash
        else turnToPlayCard
      playerNeedsToAct = player == needsToAct
      passed <- passedOrdered
      basicInfo = getGameTypeInfo(game) + getActionInfo(playerNeedsToAct, needsToAct, game, passed)
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
        game.fold("You passed.")(gameType =>
          (if (gameType == TheTable) "You passed last."
           else "") + s" The game type will be $gameType."
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
        if (newTable.morePlayersNeeded) newTable.asRight else newTable.firstRound
      }
    else "There are 3 players at this table already.".asLeft

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
      newTable <- copy(round = newRound.some).asRight
    } yield newTable

    doActionIfTurn(id, turnToMakeGameChoice, pickGameTypeOrPass, "make a game choice")
  }

  def stashCards(id: String, cards: String): Either[ErrorMessage, Table] = {

    def stash(player: Player) = for {
      round <- getRound
      cards <- Card.multiple(cards)
      tableCards <- TableCards.of(cards.toSet)
      newRound <- round.stashCards(player, tableCards)
      table <- copy(round = newRound.some).asRight
    } yield table

    doActionIfTurn(id, soloIfNeedsToStash, stash, "stash cards")
  }

  def playCard(id: String, card: String): Either[ErrorMessage, Table] = {

    def play(player: Player): Either[ErrorMessage, Table] = for {
      round <- getRound
      card <- Card.of(card)
      next <- nextAfter(player)
      newRound <- round.playCard(player, next, card)
      table <- copy(round = newRound.some).asRight
    } yield table

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

  def nextRound = for {
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
      // question is this 'sanity' check really needed here?
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
  // write a test for this that it returns a list of 8,8,8,2 sets of cards
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

  // println((Trick.of(queen(Clubs), queen(Diamonds), queen(Spades))).decideTrickWinner)
  // println((Trick.of(queen(Spades), queen(Diamonds), queen(Clubs))).decideTrickWinner)
  // println((Trick.of(nineHearts, tenHearts, aceClubs)).decideTrickWinner)
  // println((Trick.of(nineHearts, tenHearts, sevenDiamonds)).decideTrickWinner)

  // println(Trick.of(nineHearts, tenHearts, aceClubs).points)

  val testTable = Table(List(Player.of("a", "1"), Player.of("b", "2"), Player.of("c", "3")), None)
  val firstPlayer = testTable.players(0)
  println(testTable.nextAfter(firstPlayer))

}
