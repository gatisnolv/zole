package com.gatis.bootcamp.project.zole

import cats.syntax.option._
import cats.syntax.either._
import com.gatis.bootcamp.project.zole.GameChoice._

// partija
case class Round private (
  game: Option[GameType],
  playsSolo: Option[Player],
  hands: Map[Player, Set[Card]],
  tricks: List[Trick],
  // pirmā roka
  firstHand: Player,
  // kārta izspēlēt kārti
  turn: Player,
  makesGameChoice: Player,
  passed: List[Player],
  tableCards: TableCards
) {

  // round is complete if all cards have been played or if the game type is SmallZole and solo has picked up a trick
  def isComplete = tricks.length == 8 && tricks.forall(_.isComplete) ||
    game.contains(SmallZole) && playsSolo.fold(false)(solo =>
      tricks.map(_.taker).exists(_.contains(solo))
    )

  def lastTrick = tricks.headOption.fold(List.empty[(Player, Card)])(_.cardsPlayed.reverse)

  def setSolo(player: Player) = copy(playsSolo = player.some)

  def setGameType(game: GameType) = copy(game = game.some)

  def takeTableCards(player: Player): Either[ErrorMessage, Round] = hand(player).map(cards =>
    copy(tableCards = TableCards.empty, hands = hands.updated(player, cards ++ tableCards.cards))
  )

  def hand(player: Player) =
    hands.get(player).toRight(s"Unexpected error: $player's cards not found")

  def stashCards(player: Player, stash: TableCards) = hand(player).flatMap(cards =>
    if (stash.cards.forall(cards.contains(_)))
      copy(tableCards = stash, hands = hands.updated(player, cards -- stash.cards)).asRight
    else "You are trying to stash cards you don't have".asLeft
  )

  def pass(player: Player, next: Player) = copy(makesGameChoice = next, passed = player :: passed)

  def playCard(player: Player, next: Player, card: Card) = {

    def attempt(trick: Trick): Either[ErrorMessage, Trick] = for {
      first <- trick.firstCard
      hasTrump <- hand(player).map(_.exists(_.isTrump))
      hasSuit <- hand(player).map(_.exists(card => card.suit == first.suit && !card.isTrump))
      newTrick <-
        if (
          if (first.isTrump) (card.isTrump || !hasTrump)
          else (first.suit == card.suit || !hasSuit)
        ) trick.play(card, player)
        else
          s"You have to play a card of the demanded type (${if (first.isTrump) "trump"
          else s"${first.suit}"}) if you have one.".asLeft
    } yield newTrick

    def newHands = hand(player).map(cards => hands.updated(player, cards - card))

    def newTricks = tricks match {
      case Nil => (Trick.start(card, player) :: Nil).asRight
      case current :: rest =>
        if (current.isComplete) (Trick.start(card, player) :: tricks).asRight
        else attempt(current).map(_ :: rest)
    }

    def nextTurn(tricks: List[Trick]): Either[ErrorMessage, Player] =
      tricks.headOption.fold("Unexpected error: list of tricks is empty.".asLeft[Player])(trick =>
        if (trick.isComplete) trick.taker else next.asRight
      )

    if (isComplete)
      "The round is complete, no cards can be played. You may start a new round.".asLeft
    else
      hand(player).flatMap(cards => {
        if (cards.contains(card))
          for {
            hands <- newHands
            tricks <- newTricks
            turn <- nextTurn(tricks)
          } yield copy(hands = hands, tricks = tricks, turn = turn)
        else "You are trying to play a card you don't have".asLeft
      })
  }

  def score(player: Player): Either[ErrorMessage, Int] = {
    val roundIncomplete = "Scores can be calculated only once the round is complete.".asLeft[Int]
    if (isComplete)
      game.fold(roundIncomplete)(ScoreProvider.score(player, _, tricks, playsSolo, tableCards))
    else roundIncomplete
  }

}

object Round {
  def start(
    first: Player,
    hands: Map[Player, Set[Card]],
    tableCards: Set[Card]
  ): Either[ErrorMessage, Round] =
    TableCards.of(tableCards) map (Round(None, None, hands, Nil, first, first, first, Nil, _))
}
