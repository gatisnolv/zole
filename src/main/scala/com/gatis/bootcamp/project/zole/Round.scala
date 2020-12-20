package com.gatis.bootcamp.project.zole

import cats.syntax.option._
import cats.syntax.either._
import cats.syntax.traverse._
import com.gatis.bootcamp.project.zole.GameChoice._

case class Round private (
  game: Option[GameType],
  playsSolo: Option[Player],
  // while a trick is not complete played cards are kept in players hands to decide winner after completion
  hands: Map[Player, Set[Card]],
  tricks: List[Trick],
  // pirmā roka - stays constant for the round
  firstHand: Player,
  turn: Player,
  makesGameChoice: Player,
  passed: List[Player],
  tableCards: TableCards
) {

  // the last trick is either the last complete trick if new one has not yet started or the cards forming the new one
  // def lastTrick =
  //   tricks.headOption.fold(Set.empty[Card])(_.cardsPlayed.map({ case (_, card) => card }).toSet)

  // round is complete if all cards have been played or if the game type is SmallZole and solo has picked up a trick
  def isComplete = tricks.length == 8 && tricks.forall(_.isComplete) ||
    game.contains(SmallZole) && playsSolo.fold(false)(solo =>
      tricks.map(_.winner).exists(_.contains(solo))
    )

  private def calculateScores: Either[ErrorMessage, Map[Player, Int]] = {
    val roundIncomplete = "Scores can be calculated only once the round is complete.".asLeft
    if (isComplete)
      game.fold(roundIncomplete)(gameType =>
        gameType match {
          // TODO implement score calculation
          case Big       => ???
          case Zole      => ???
          case SmallZole => ???
          case TheTable  => ???
        }
      )
    else roundIncomplete
  }

  def score(player: Player) =
    calculateScores.flatMap(_.get(player).toRight(s"Unexpected error: $player's score not found"))

  // def whoPlayedCardInCurrentTrick(card: Card): Either[ErrorMessage, Player] = tricks.headOption
  //   .toRight("No cards have been played in this round yet")
  //   .flatMap(_.whoPlayed(card))

  def whoPlayedWhatInCurrentTrickOrdered =
    tricks.headOption.fold(List.empty[(Player, Card)])(_.cardsPlayed.reverse)

  private def cardPlayedInCurrentTrick(player: Player) = tricks.headOption.fold(Option.empty[Card])(
    _.cardsPlayed.find { case (playedBy, _) => playedBy == player }.map { case (_, card) => card }
  )

  def setSolo(player: Player) = copy(playsSolo = Some(player))

  def setGameType(game: GameType) = copy(game = Some(game))

  def takeTableCards(player: Player): Either[ErrorMessage, Round] =
    playersCards(player).map(cards =>
      copy(tableCards = TableCards.empty, hands = hands.updated(player, cards ++ tableCards.cards))
    )

  private def playersCards(player: Player) =
    hands.get(player).toRight(s"Unexpected error: $player's cards not found")

  def playersHand(player: Player) =
    playersCards(player).map(cards => cardPlayedInCurrentTrick(player).fold(cards)(cards - _))

  def stashCards(player: Player, stash: TableCards) = playersCards(player).flatMap(cards =>
    if (stash.cards.forall(cards.contains(_)))
      copy(tableCards = stash, hands = hands.updated(player, cards -- stash.cards)).asRight
    else "You are trying to stash cards you don't have".asLeft
  )

  def pass(player: Player, next: Player) = copy(makesGameChoice = next, passed = player :: passed)

  // TODO could simplify a bit in the sense that, since tricks now keep players who played cards,
  // no point to keep the current trick cards in players hands while trick is ongoing any longer
  def playCard(player: Player, next: Player, card: Card) = {

    def newTricks = tricks match {
      case Nil => (Trick.start(card, player) :: Nil).asRight
      case current :: rest =>
        if (current.isComplete) (Trick.start(card, player) :: tricks).asRight[ErrorMessage]
        else current.add(card, player).map(_ :: rest)
    }

    def newHands(tricks: List[Trick]) = tricks.headOption.fold(hands.asRight[ErrorMessage])(trick =>
      if (trick.isComplete)
        hands.toList
          .foldLeft(List.empty[Either[ErrorMessage, (Player, Set[Card])]])({
            case (acc, (player, _)) => playersHand(player).map((player, _)) :: acc
          })
          .sequence
          .map(_.toMap)
      else hands.asRight
    )

    def nextTurn(tricks: List[Trick]): Either[ErrorMessage, Player] =
      tricks.headOption.fold("Unexpected error: list of tricks is empty.".asLeft[Player])(trick =>
        if (trick.isComplete) trick.winner else next.asRight
      )

    if (isComplete)
      "The round is complete, no cards can be played. You may start a new round.".asLeft
    else
      playersCards(player).flatMap(cards => {
        if (cards.contains(card))
          for {
            tricks <- newTricks
            hands <- newHands(tricks)
            turn <- nextTurn(tricks)
          } yield copy(hands = hands, tricks = tricks, turn = turn)
        else "You are trying to play a card you don't have".asLeft
      })
  }

}

object Round {
  def start(
    first: Player,
    hands: Map[Player, Set[Card]],
    tableCards: Set[Card]
  ): Either[ErrorMessage, Round] = for {
    tableCards <- TableCards.of(tableCards)
  } yield Round(None, None, hands, Nil, first, first, first, Nil, tableCards)
}
