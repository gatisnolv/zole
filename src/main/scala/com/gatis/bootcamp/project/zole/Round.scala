package com.gatis.bootcamp.project.zole

import cats.syntax.option._
import cats.syntax.either._
import cats.syntax.traverse._

case class Round private (
  game: Option[GameType],
  // remember to change playsSolo (lielais) role between rounds
  playsSolo: Option[Player],
  // could have Map[Player, (Set[Card], Boolean)] to designate whether picked up yet
  hands: Map[Player, Set[Card]],
  // tracks tricks, the respective taker
  tricks: List[(Trick, Option[Player])],
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

  // the last trick is either the last complete trick if new one has not yet started or the cards forming the new one
  def lastTrick = tricks.headOption.fold(Set.empty[Card])({ case (trick, _) => trick.cards.toSet })

  // likely to be subsumed by playCard. maybe with this also store points/tricks for the players
  // def saveTrick(trick: Trick) = ??? // copy(tricks = trick :: tricks)

  // def firstCardOfCurrentTrick = currentTrick.lastOption

  def whoPlayedCardInCurrentTrick(card: Card): Either[ErrorMessage, Player] = hands
    .find({ case (player, cards) => cards.contains(card) })
    .map({ case (player, _) => player })
    .toRight("This card was not played in the current trick.")

  def whoPlayedWhatInCurrentTrickInOrder =
    tricks.headOption.fold(List.empty[(Player, Card)].asRight[String])({ case (trick, _) =>
      trick.cards.reverse.map(card => whoPlayedCardInCurrentTrick(card).map((_, card))).sequence
    })

  def setSolo(player: Player) = copy(playsSolo = Some(player))

  def setGameType(game: GameType) = copy(game = Some(game))

  def takeTableCards(player: Player): Either[ErrorMessage, Round] =
    playersCards(player).map(cards =>
      copy(tableCards = TableCards.empty, hands = hands.updated(player, cards ++ tableCards.cards))
    )

  def playersCards(player: Player) = hands
    .get(player)
    .fold(s"Unexpected error: $player's cards not found".asLeft[Set[Card]])(_.asRight)

  def stashCards(player: Player, stash: TableCards) = playersCards(player).flatMap(cards =>
    if (stash.cards.forall(cards.contains(_)))
      copy(tableCards = stash, hands = hands.updated(player, cards -- stash.cards)).asRight
    else "You are trying to stash cards you don't have".asLeft
  )

  def pass(next: Player) = copy(makesGameChoice = next, playersPassed = playersPassed + 1)

  // TODO should also set up for next trick, inform of the trick winner
  // while trick is not complete, leave the card in players hand
  def playCard(player: Player, next: Player, card: Card) = playersCards(player).flatMap(cards => {

    def trickWinner(trick: Trick) =
      if (trick.isComplete)
        for {
          player <- trick.decideWinner.flatMap(whoPlayedCardInCurrentTrick(_))
        } yield player.some
      else None.asRight

    if (cards.contains(card)) {
      val newTrickEither =
        tricks.headOption.fold((Trick.start(card), Option.empty[Player]).asRight[ErrorMessage])({
          case (trick, _) =>
            if (trick.isComplete) (Trick.start(card), None).asRight
            else
              for {
                newTrick <- trick.add(card)
                winner <- trickWinner(trick)
              } yield (newTrick, winner)
        })

      // TODO implement removal of current trick cards from players hand cards if trick was completed in this move

      newTrickEither.flatMap(newTrick => copy(tricks = newTrick :: tricks).asRight[ErrorMessage])
    } else "You are trying to play a card you don't have".asLeft
  })

  // TODO maybe have this, so at the end of a round players can review the tricks (at least the roundscore) before the next one starts
  // likely to be subsumed by play card after a complete round
  // def nextRound=

}

object Round {
  def start(
    first: Player,
    hands: Map[Player, Set[Card]],
    tableCards: Set[Card]
  ): Either[ErrorMessage, Round] = for {
    tableCards <- TableCards.of(tableCards)
  } yield Round(None, None, hands, Nil, first, first, first, 0, tableCards)
}
