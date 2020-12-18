package com.gatis.bootcamp.project.zole

import cats.syntax.option._
import cats.syntax.either._
import cats.syntax.traverse._

case class Round private (
  game: Option[GameType],
  // remember to change playsSolo (lielais) role between rounds
  playsSolo: Option[Player],
  // could have Map[Player, (Set[Card], Boolean)] to designate whether picked up yet
  eachPlayersCards: Map[Player, Set[Card]],
  // tracks tricks, the respective taker
  tricks: List[(Trick, Player)],
  // beware of ordering
  currentTrick: List[Card],
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
  def lastTrick =
    if (currentTrick.nonEmpty) currentTrick.some
    else
      tricks match {
        case Nil          => None
        case (t, _) :: ts => t.toSet.some
      }

  // likely to be subsumed by playCard. maybe with this also store points/tricks for the players
  def saveTrick(trick: Trick) = ??? // copy(tricks = trick :: tricks)

  def firstCardOfCurrentTrick = currentTrick.lastOption

  def whoPlayedCardInCurrentTrick(card: Card): Either[ErrorMessage, Player] = eachPlayersCards
    .find({ case (player, cards) => cards.contains(card) })
    .map({ case (player, _) => player })
    .toRight("This card was not played in the current trick.")

  def whoPlayedWhatInCurrentTrickInOrder = currentTrick.reverse
    .map(card => whoPlayedCardInCurrentTrick(card).map((_, card)))
    .sequence

  def setSolo(player: Player) = copy(playsSolo = Some(player))

  def playersCards(player: Player) = eachPlayersCards
    .get(player)
    .fold(s"Unexpected error: $player's cards not found".asLeft[Set[Card]])(_.asRight)

  def stashCards(player: Player, stash: TableCards) = playersCards(player).map(cards =>
    copy(
      tableCards = stash,
      eachPlayersCards = eachPlayersCards.updated(player, cards -- stash.cards)
    )
  )

  def takeTableCards(player: Player): Either[ErrorMessage, Round] = playersCards(player).map(
    cards =>
      setSolo(player).copy(
        tableCards = TableCards.empty,
        eachPlayersCards = eachPlayersCards.updated(player, cards ++ tableCards.cards)
      )
  )

  def pass(next: Player) = copy(makesGameChoice = next, playersPassed = playersPassed + 1)

  def setGameType(game: GameType) = copy(game = Some(game))

  // TODO should also set up for next trick, inform of the trick winner
  def playCard(next: Player, card: Card) = copy(currentTrick = card :: currentTrick)

  // TODO maybe have this, so at the end of a round players can review the tricks (at least the roundscore) before the next one starts
  // def nextRound=

}

object Round {
  def start(
    first: Player,
    eachPlayersCards: Map[Player, Set[Card]],
    tableCards: Set[Card]
  ): Either[ErrorMessage, Round] = for {
    tableCards <- TableCards.of(tableCards)
  } yield Round(None, None, eachPlayersCards, Nil, Nil, first, first, first, 0, tableCards)

}
