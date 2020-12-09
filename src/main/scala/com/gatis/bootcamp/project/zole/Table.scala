package com.gatis.bootcamp.project.zole

import scala.util.Random
import com.gatis.bootcamp.project.zole.Rank._
import com.gatis.bootcamp.project.zole.Suit._
import com.gatis.bootcamp.project.zole.Card._
// import cats.implicits._
// import cats.syntax._
import cats.syntax.either._
import cats.syntax.option._
import com.gatis.bootcamp.project.zole.GameChoice._

case class TableCards(cards: Set[Card])
object TableCards {
  def of(cards: Set[Card]): Either[ErrorMessage, TableCards] =
    if (cards.size != 2) s"There should be exactly two table cards, got ${cards.size}".asLeft
    else TableCards(cards).asRight
}

//stiķis
case class Trick(cards: List[Card]) {
  def toSet = cards.toSet
  def containsTrump = cards.exists(_.isTrump)
  def points = cards.foldLeft(0)((acc, card) => acc + card.points)

  //noteikt stiķa uzvarētāju
  def decideTrickWinner: Card = {
    if (containsTrump) cards.max
    //for non-trump cards, the round is decided by the strongest card of demanded suit, so order here is important
    else cards.filter(_.suit == cards(0).suit).max
  }

}

object Trick {
  def of(card1: Card, card2: Card, card3: Card) = Trick(card1 :: card2 :: card3 :: Nil)
}

// spēles veids
sealed trait GameType
// spēlētāja izvēle
sealed abstract class GameChoice private (val shortName: String)

object GameChoice {
  // lielais = pacelt galda kārtis
  case object Big extends GameChoice("B") with GameType
  // zole
  case object Zole extends GameChoice("Z") with GameType
  // mazā zole
  case object SmallZole extends GameChoice("S") with GameType
  // garām
  case object Pass extends GameChoice("P")
  // galdiņš
  case object TheTable extends GameType

  def getGameType(choice: String, playersPassed: Int): Either[ErrorMessage, Option[GameType]] =
    choice match {
      case Big.shortName       => Big.some.asRight
      case Zole.shortName      => Zole.some.asRight
      case SmallZole.shortName => SmallZole.some.asRight
      // 3 passes means galdiņš
      case Pass.shortName => (if (playersPassed < 2) None else TheTable.some).asRight
      case _              => s"Invalid value for game choice: $choice".asLeft
    }
}

//roundPoints - acis (partijas punkti), score - spēles punkti
case class Player private (name: String, id: String, score: Int) {
  def updateScore(points: Int) = copy(score = score + points)
  // def increaseTrickCount = ??? // priekš "galdiņa" spēles
}

object Player {
  def of(name: String, id: String) = Player(name, id, 0)
}

// remember to change playsSolo (lielais) role between rounds
case class Round private (
  game: Option[GameType],
  playsSolo: Option[Player],
  // could have Map[Player, (Set[Card], Boolean)] to designate whether picked up yet
  playersCards: Map[Player, Set[Card]],
  //tracks tricks, the respective taker
  tricks: List[(Trick, Player)],
  currentTrick: Set[Card],
  // pirmā roka - stays constant for the round
  firstHand: Player,
  // kam jāliek kārts
  turn: Player,
  // player who chooses game type
  makesGameChoice: Player,
  // to track how many passed (garām), because The Table starts once every player passes, so we don't loop more than once
  playersPassed: Int,
  tableCards: TableCards
) {

  // the last trick is the last complete trick if new one has not yet started or the cards forming the new one
  def lastTrick =
    if (currentTrick.nonEmpty) currentTrick.some
    else
      tricks match {
        case Nil          => None
        case (t, _) :: ts => t.toSet.some
      }

  // likely to be replaced by playCard. maybe with this also store points/tricks for the players
  def saveTrick(trick: Trick) = ??? // copy(tricks = trick :: tricks)

  def stashCards(cards: TableCards) = copy(tableCards = cards).some

  def setSolo(player: Player) = copy(playsSolo = Some(player)).some

  def pass(next: Player) = copy(makesGameChoice = next, playersPassed = playersPassed + 1).some

  def setGameType(game: GameType) = copy(game = Some(game)).some

  // TODO should also set up for next trick, inform of the trick winner
  def playCard(next: Player, card: Card) = copy(currentTrick = currentTrick + card).some

  // TODO maybe like this, so at the end of a round players can review the tricks
  // def nextRound=

}

object Round {
  def start(first: Player, playersCards: Map[Player, Set[Card]], tableCards: Set[Card]): Round =
    // use smart constructors in for comprehensions for table cards, player round cards
    ???
  // Round(None, None, playersCards, Nil, Set.empty, first, first, 0, tableCards)
}

// first - kam pirmā roka, rotē pēc partijas (round)
case class Table private (val players: List[Player], val round: Option[Round]) {
  def next(player: Player) = players match {
    case Nil => "There are no players at the table".asLeft
    // using modulo of seated player count instead of 3, means this fn works even when there are less than 3 seated players
    case _ => players((players.indexOf(player) + 1) % players.length).asRight
  }

  def hasPlayerNamed(name: String) = players.exists(_.name == name)

  def playersNeeded = 3 - players.length

  def seatPlayer(name: String, id: String): Either[ErrorMessage, Table] =
    if (players.length < 3)
      if (hasPlayerNamed(name))
        s"There is already someone at the table with name $name, please use a different name.".asLeft
      else {
        // a choice to have the first seated be the starting player, so appending here
        val newTable = copy(players = players :+ Player.of(name, id))
        (if (players.length < 3) newTable else newTable.firstRound).asRight
        newTable.asRight
      }
    else "There are 3 players at this table already.".asLeft

  def makeGameChoice(id: String, choice: String): Either[ErrorMessage, Table] = {

    def pickGameTypeOrPass(choice: String, round: Round, player: Player) = for {
      gameType <- GameChoice.getGameType(choice, round.playersPassed)
      newTable <- gameType.fold(
        next(player).flatMap(next => copy(round = round.pass(next)).asRight)
      )(gameType => {
        val roundWithGameType = round.setGameType(gameType)
        (if (gameType == TheTable) copy(round = roundWithGameType)
         else copy(round = roundWithGameType.flatMap(_.setSolo(player)))).asRight
      })
    } yield newTable

    round match {
      case Some(round) =>
        round.game match {
          case None =>
            players.find(_.id == id) match {
              case Some(player) =>
                if (round.makesGameChoice == player)
                  pickGameTypeOrPass(choice, round, player)
                else
                  s"It's not your turn to make a game choice, it's ${round.makesGameChoice}'s turn.".asLeft
              case None => s"Player with id $id is not sitting at this table.".asLeft
            }
          case Some(game) =>
            s"This rounds game type has already been determined as ${game} by ${round.makesGameChoice}.".asLeft
        }
      case None => s"Need ${3 - players.length} more players to start playing.".asLeft
    }
  }

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
        copy(round = Round.start(first, playersCards, deal.last).some).asRight
      }
      // This should be a 5xx
      else "Unexpected error dealing cards".asLeft

    }
}

// extends App for quick testing
object Table extends App {
  //write a test for this that it returns a list of 8,8,8,2 sets of cards
  //store deal for game
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
  println(testTable.next(firstPlayer))

}
