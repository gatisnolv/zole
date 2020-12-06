package com.gatis.bootcamp.project.zole

import scala.util.Random
import com.gatis.bootcamp.project.zole.Rank._
import com.gatis.bootcamp.project.zole.Suit._
import cats.implicits._

case class TableCards(card1: Card, card2: Card)

//stiķis
case class Trick(card1: Card, card2: Card, card3: Card) {
  def cardList = card1 :: card2 :: card3 :: Nil
  def containTrump = cardList.exists(_.isTrump)
  def points = cardList.foldLeft(0)((acc, card) => acc + card.points)
}

// spēles veids
sealed abstract class Game private (val shortName: String)

object Game {
  // lielais
  case object Big extends Game("B")
  // zole
  case object Zole extends Game("Z")
  // mazā zole
  case object SmallZole extends Game("S")
  // galdiņš
  case object TheTable extends Game("T")

  def of(game: String): Either[ErrorMessage, Game] = game match {
    case Big.shortName       => Big.asRight
    case Zole.shortName      => Zole.asRight
    case SmallZole.shortName => SmallZole.asRight
    case TheTable.shortName  => TheTable.asRight
    case _                   => s"Invalid value for game type: $game".asLeft
  }
}

//roundPoints - acis (partijas punkti), score - spēles punkti
case class Player private (
  name: String,
  id: String,
  cards: Set[Card],
  cardPlayed: Option[Card],
  roundPoints: Int,
  score: Int,
  trickCount: Option[Int]
) {
  def updateScore(points: Int) = this.copy(score = score + points)
  // def increaseTrickCount = ??? // priekš "galdiņa" spēles
}

object Player {
  def of(name: String, id: String) = Player(name, id, Set.empty, None, 0, 0, None)
}

// remember to change playsSolo (lielais) role between rounds
// whoseTurn - kam jāliek kārts
case class Round private (
  game: Game,
  playsSolo: Option[Player],
  tricks: List[Trick],
  whoseTurn: Player,
  // to track how many passed (garām), because The Table starts once every player passes, so we don't loop more than once
  playersPassed: Int,
  tableCards: TableCards
  // whoseTurnToChooseGameType - not sure if needed yet
) {
  def lastTrick = tricks.headOption

  // maybe with this also store points/tricks for the players
  def saveTrick(trick: Trick) = this.copy(tricks = trick :: tricks)

  //norakt kārtis
  def stashCards(cards: TableCards) = this.copy(tableCards = cards)

  def setSolo(player: Player) = this.copy(playsSolo = Some(player))

}

object Round {
  def of(game: Game, playsSolo: Option[Player], first: Player, tableCards: TableCards) =
    Round(game, playsSolo, Nil, first, 0, tableCards)
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
        this.copy(players = Player.of(name, id) :: players).asRight
    else "There are 3 players at this table already.".asLeft

  def firstRound = ??? // use newRound

  def newRound(
    game: Game,
    playsSolo: Option[Player],
    first: Player,
    tableCards: TableCards
  ): Either[ErrorMessage, Table] =
    if (players.length < 3)
      s"3 players needed to play, there are now only ${players.length}.".asLeft
    else {
      val deal = Table.dealCards
      if (deal.nonEmpty && deal.init.forall(_.size == 8) && deal.last.size == 2) {
        // q about use of init, last with regards to exception throwing, I guess, could wrap with Try
        val tableCards = deal.last.toList
        val updatedPlayers = (players zip deal.init).map({ case (player, cards) =>
          player.copy(cards = cards)
        })
        this
          .copy(
            players = updatedPlayers,
            round = Some(Round.of(game, playsSolo, first, TableCards(tableCards(0), tableCards(1))))
          )
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

  //noteikt stiķa uzvarētāju
  def decideTrickWinner(cards: Trick): Card = {
    if (cards.containTrump) cards.cardList.max
    //for non-trump cards, the round is decided by the strongest card of demanded suit
    else cards.cardList.filter(_.suit == cards.card1.suit).max
  }

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

  println(decideTrickWinner(Trick(queen(Clubs), queen(Diamonds), queen(Spades))))
  println(decideTrickWinner(Trick(queen(Spades), queen(Diamonds), queen(Clubs))))
  println(decideTrickWinner(Trick(nineHearts, tenHearts, aceClubs)))
  println(decideTrickWinner(Trick(nineHearts, tenHearts, sevenDiamonds)))

  println(Trick(nineHearts, tenHearts, aceClubs).points)

  val testTable = Table(
    List(
      Player("a", "1", Set.empty, None, 0, 0, None),
      Player("b", "2", Set.empty, None, 0, 0, None),
      Player("c", "3", Set.empty, None, 0, 0, None)
    ),
    None
  )
  val firstPlayer = testTable.players(0)
  println(testTable.next(firstPlayer))

}
