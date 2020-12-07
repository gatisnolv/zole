package com.gatis.bootcamp.project.zole

import scala.util.Random
import com.gatis.bootcamp.project.zole.Rank._
import com.gatis.bootcamp.project.zole.Suit._
// import cats.implicits._
import cats.syntax.either._
import cats.syntax.option._

case class TableCards(cards: Set[Card])
object TableCards {
  def of(cards: Set[Card]): Either[ErrorMessage, TableCards] =
    if (cards.size != 2) s"There should be exactly two table cards, got ${cards.size}".asLeft
    else TableCards(cards).asRight
}

//stiķis
case class Trick(card1: Card, card2: Card, card3: Card) {
  private def cardList = card1 :: card2 :: card3 :: Nil
  def toSet = cardList.toSet
  def containTrump = cardList.exists(_.isTrump)
  def points = cardList.foldLeft(0)((acc, card) => acc + card.points)

  //noteikt stiķa uzvarētāju
  def decideTrickWinner: Card = {
    if (containTrump) cardList.max
    //for non-trump cards, the round is decided by the strongest card of demanded suit
    else cardList.filter(_.suit == card1.suit).max
  }
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
// turn - kam jāliek kārts
case class Round private (
  game: Option[GameType],
  playsSolo: Option[Player],
  playersCards: Map[Player, Set[Card]],
  //tracks tricks and the respective winner
  tricks: List[(Trick, Player)],
  currentTrick: Set[Card],
  turn: Player,
  // to track how many passed (garām), because The Table starts once every player passes, so we don't loop more than once
  turnToMakeGameChoice: Player,
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

  def stashCards(cards: TableCards) = copy(tableCards = cards)

  def setSolo(player: Player) = copy(playsSolo = Some(player))

  def pass(next: Player) = copy(turnToMakeGameChoice = next, playersPassed = playersPassed + 1)

  def setGameType(game: GameType) = copy(game = Some(game))

  // TODO should also set up for next trick, inform of the trick winner
  def playCard(next: Player, card: Card) = copy(currentTrick = currentTrick + card)

}

object Round {
  def start(first: Player, playersCards: Map[Player, Set[Card]], tableCards: Set[Card]) =
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
      else
        copy(players = Player.of(name, id) :: players).asRight
    else "There are 3 players at this table already.".asLeft

  def gameChoice(): Either[ErrorMessage, Table] = ???

  def firstRound = ??? // use newRound

  def newRound(
    game: GameType,
    playsSolo: Option[Player],
    first: Player,
    tableCards: TableCards
  ): Either[ErrorMessage, Table] =
    if (players.length < 3)
      s"3 players needed to play, there are now only ${players.length}.".asLeft
    else {
      val deal = Table.dealCards
      //question is this 'sanity' check really needed here?
      if (deal.nonEmpty && deal.init.forall(_.size == 8)) {
        // q about use of init, last with regards to exception throwing, I guess, could wrap with Try
        val playersCards = (players zip deal.init).toMap
        this
          .copy(round = Some(Round.start(first, playersCards, deal.last)))
          .asRight
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

  def allCards = {
    val eithers = for {
      suit <- Suit.ordered
      rank <- Rank.ordered
      // } yield Card.of(rank.toString + suit.toString)
      // for easier visual checking, when suits toString defined as emoji
    } yield Card.of(rank.toString + suit.character.toString)

    eithers.foldLeft(Set.empty[Card])((acc, el) =>
      el match {
        case Right(card) => acc + card
        case Left(_)     => acc
      }
    )
  }

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

  println((Trick(queen(Clubs), queen(Diamonds), queen(Spades))).decideTrickWinner)
  println((Trick(queen(Spades), queen(Diamonds), queen(Clubs))).decideTrickWinner)
  println((Trick(nineHearts, tenHearts, aceClubs)).decideTrickWinner)
  println((Trick(nineHearts, tenHearts, sevenDiamonds)).decideTrickWinner)

  println(Trick(nineHearts, tenHearts, aceClubs).points)

  val testTable = Table(List(Player.of("a", "1"), Player.of("b", "2"), Player.of("c", "3")), None)
  val firstPlayer = testTable.players(0)
  println(testTable.next(firstPlayer))

}
