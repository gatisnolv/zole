# Zole

## Evolution Scala bootcamp course project repository.

This is a game server for the card game Zole, a popular card game in Latvia (https://en.wikipedia.org/wiki/Zole). The rules can be found on the following site - http://www.zolmaniem.lv/cms/page/view/21#Zoles_noteikumi. Unfortunately, the English wikipedia article is very lacking and incomplete in describing the rules, so can be used only as a general description, but not to fully understand the game. I'll include a summary here.

The game is unique to Latvia, though similar games exist. There are a number of terms specific to the game which I've had to translate for use in code, so the code includes some comments with the Latvian names for such terms since the English versions are non-standard and a necessary by-product of this project. I include a glossary of these terms here in the readme as well.

## Glossary:

Trick - stiķis

Trump - trumpe

Round points - acis

Score points - spēles punkti

Big - Lielais

Zole - Zole

Small zole - Mazā zole

The table - Galdiņš

Pass - garām

Round - partija

Solo player - lielais spēlētājs (tas, kurš spēlē viens)

Solo players opponents - lielā pretinieki (mazie)

First hand - pirmā roka

Table cards - galda kārtis

Stash - norakt kārtis

## Rules summary:

It is a trick-taking card game (https://en.wikipedia.org/wiki/Trick-taking_game). 26 cards are used (9 to Ace of all suits + 7 ♦ and 8 ♦). There are trump and non-trump cards with defined strengths. It is turn-based and played among 3 players.

A complete trick consists of 3 cards. The player playing the first card of a trick may play any card, the following players must respond by each playing a card of the same type if they have any, otherwise they may play any other card. A trump card requires trump cards and a non-trump requires a non-trump card of the same suit. The trick is taken by the player who played the strongest trump among the three cards if there was a trump, otherwise it is taken by the player, who played the strongest non-trump card.

**Strengths of trump cards:**

Q♣ > Q♠ > Q♥ > Q♦ > J♣ > J♠ > J♥ > J♦ > A♦ > T♦ > K♦ > 9♦ > 8♦ > 7♦

**Strengths of non-trump cards:**

A♣ > T♣ > K♣ > 9♣

A♠ > T♠ > K♠ > 9♠

A♥ > T♥ > K♥ > 9♥

**Points:**

A=11, T=10, K=4, Q=3, J=2, (9=0, 8=0, 7=0)

The game proceeds in rounds. At the start of the round every player is dealt 8 cards and the 2 remaining are left on the table. The round starts with a game choice offered to players going clockwise around the table until a game type is determined. A player can either declare a game type (Zole, Big, Small zole) or pass. If the player passes, the next player gets to make a game choice. If all three players pass, the game type "The table" is played. The first card of the round is played by the player who first had the opportunity to make a game choice whether they chose a game type or passed. Each following trick is started by the player who took the previous trick. The round is complete after the players play 8 tricks (at which point they're out of cards). For each next round the player who first got to make a game choice is shifted clockwise.

In all of Zole, Big and Small zole game types, the player who chose the game type plays solo against the other two players (the tricks taken by the two players are added together, both get the point total). In "The table" everybody plays on their own.

In the game type Big, the player choosing this game type picks up both of the table cards adding them to his hand and chooses two cards of his hands to stash. (The points of these cards are added to their point total at the end of the round.) In Zole and Small zole, these go to the solo players opponents, and in "The table" they're not used.

The objectives in the game types are as follows:

Zole, Big - solo wins if gets more than half of all points

Small zole - solo wins if takes 0 tricks, otherwise loses

The table - taker of most tricks (points if multiple players have the same amount of most tricks) is the loser.

**Scoring:**

Once a round is complete score points are determined and added to each players score total (the score is kept across rounds). The scoring table is quite detailed (http://www.zolmaniem.lv/cms/page/view/21#PUNKTU_UZSKAITES_TABULA):
![alt text](https://github.com/gatisnolv/zole/blob/main/docs/scoring.png "Scoring table")

## Running the server:

The server can be started by executing `sbt run` from the project root directory.

## Endpoints:

**GET /** - a basic 'hello' endpoint.

**POST /new** - creates a new table, returning a three letter table code to use for sitting (registering at a table).

**POST /register** - used for taking a seat (registering) at a table, expects JSON with fields 'code' - the table code obtained previously, 'name' - unique (for the table) player name. Three players are required to continue with the game. This request sets two cookies: 'code' - the table code, 'uuid' - uuid for player. These are used to identify the table and the player for subsequent communication with the server.

**GET /turn** - information about whose turn it is to take action, the played cards in the current trick, the game type, and the solo player.

**GET /hand** - the players current hand.

**POST /choice** - used to make a game choice, passed as a path parameter, e.g. /choice/B to choose game type Big. Possible game choice values: B - Big, Z - Zole, S - Small zole, P - pass.

**POST /stash** - used to stash the cards when playing Big, passed as a path parameter, e.g. /stash/Ac,Ts to stash the ace of clubs and ten of spades. Returns the new hand (minus the stashed cards) if successful.

**POST /play** - used to play the card, passed as a path parameter, e.g. /play/Qc to play the queen of clubs. Returns the new hand (minus the played card) if successful.

**GET /turnOrder** - informs of the sitting order of players at the table.

**GET /score** - informs of the scores of the players at the table.

**POST /nextRound** - used to start the next round, can be used only after the ongoing round has been completed.

## To-do ideas:

During the time available for the projects I was able to fully implement the game logic and have a working game server. But there are multiple ways this project could be improved upon:

_Persistence_ - the game state is kept in memory, this was meant to be a stop-gap solution originally, I did not have enough time to change this.

_More RESTful_ - the server uses appropriate HTTP methods, but the data exchange is text based. For a more realistic backend it would make sense to have JSON with appropriate structure to facilitate parsing for clients. The endpoints could be refactored to use resource-based addressing.

_WebSocket_ - since the game is turn based it would be convenient to have updates about the game state come in as they become available. Currently polling would be needed.

_Tests_ - I did not have enough time to write comprehensive tests

_Uncommon game variants_ - the exist some more uncommon game variants that could be implemented

_UI frontend_ - it would be nice to produce a UI frontend client for the server at which point the game could be offered to end users (players) as a complete game to play with others.

## Presentation slides:

The slides are available here: https://github.com/gatisnolv/zole/blob/main/docs/Project%20presentation%20-%20Zole.pptx
