package com.gatis.bootcamp.project.zole

import scala.util.Random
import com.gatis.bootcamp.project.zole.Rank._
import com.gatis.bootcamp.project.zole.Suit._
import cats.implicits._

case class StashedCards(card1: Card, card2: Card)

//stiķis
case class Trick(card1: Card, card2: Card, card3: Card) {
  def cardList = card1 :: card2 :: card3 :: Nil
  def containTrump = cardList.exists(_.isTrump)
  def points = cardList.foldLeft(0)((acc, card) => acc + card.points)
}

// spēles veids
sealed trait Game
// lielais
case object Big extends Game
// zole
case object Zole extends Game
// mazā zole
case object SmallZole extends Game
// galdiņš
case object TheTable extends Game

//roundPoints - acis (partijas punkti), score - spēles punkti
case class Player private (
  name: String,
  id: String,
  cards: List[Card],
  roundPoints: Int,
  score: Int,
  trickCount: Option[Int]
) {
  def updateScore(points: Int) = this.copy(score = score + points)
  // def increaseTrickCount = ???
}

// remember to change playsSolo (lielais) role between rounds
// whose turn - kam jāliek kārts
case class Round private (
  game: Game,
  playsSolo: Option[Player],
  tricks: List[Trick],
  whoseTurn: Player
  //whoseTurnToChooseGameType - not sure if needed yet
) {
  def lastTrick = tricks.headOption
  def saveTrick(trick: Trick) = this.copy(tricks = trick :: tricks)
}

object Round {
  def of(game: Game, solo: Option[Player], first: Player) = Round(game, solo, Nil, first)
}

// first - kam pirmā roka, rotē pēc partijas (round)
case class Table private (val players: List[Player], val round: Option[Round]) {
  def next(player: Player) = players match {
    case Nil => "There are no players at the table".asLeft
    // using modulo of seated player count instead of 3, means this fn works even when there are not yet 3 seated players
    case _ => players((players.indexOf(player) + 1) % players.length).asRight
  }

  def seatPlayer(player: Player): Either[ErrorMessage, Table] =
    if (players.length < 3) this.copy(players = player :: players).asRight
    else "There are 3 players at this table already".asLeft

  def empty = Table(Nil, None)

  def newRound(game: Game, solo: Option[Player], first: Player): Either[ErrorMessage, Table] =
    if (players.length != 3) "3 players needed to start playing".asLeft
    else this.copy(round = Some(Round.of(game, solo, first))).asRight
}

// extends App for quick testingb
object Table extends App {
  //write a test for this that it returns a list of 8,8,8,2 sets of cards
  def dealCards = {
    val deal = Random.shuffle(allCards.toList).sliding(8, 8).toList.map(_.toSet)
    //store deal for game
    deal
  }

  def arrangeCardsInHand(cards: Set[Card]) = cards.toList.sorted(Card.InHandPrettyOrdering).reverse

  //norakt kārtis
  def stashCards(cards: StashedCards) = {
    // store stashed cards for player
    ???
  }

  //noteikt stiķa uzvarētāju
  def decideRoundWinner(cards: Trick): Card = {
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

  println(decideRoundWinner(Trick(queen(Clubs), queen(Diamonds), queen(Spades))))
  println(decideRoundWinner(Trick(queen(Spades), queen(Diamonds), queen(Clubs))))
  println(decideRoundWinner(Trick(nineHearts, tenHearts, aceClubs)))
  println(decideRoundWinner(Trick(nineHearts, tenHearts, sevenDiamonds)))

  println(Trick(nineHearts, tenHearts, aceClubs).points)

  val testTable = Table(
    List(
      Player("a", "1", Nil, 0, 0, None),
      Player("b", "2", Nil, 0, 0, None),
      Player("c", "3", Nil, 0, 0, None)
    ),
    None
  )
  val firstPlayer = testTable.players(0)
  println(testTable.next(firstPlayer))

}
