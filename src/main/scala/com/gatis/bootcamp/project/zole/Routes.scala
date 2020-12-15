package com.gatis.bootcamp.project.zole

import cats.effect.{IO}
import org.http4s.dsl.io._
import org.http4s._
import cats.syntax.all._
import org.http4s.implicits._
import scala.util.Random
import java.util.UUID
import com.gatis.bootcamp.project.cache.ExpiringCache.Cache

case class CardR(rank: String, suit: String)

case class Registration(code: String, name: String)

object Routes {

  private[zole] def httpApp(tables: Cache[IO, String, Table]) = {

    // 3 letter code supports (26^3) 17576 games
    def getNewGameCode: IO[String] = {
      // check for uniqueness
      def generateCode = Random.alphanumeric.filter(_.isLetter).take(3).mkString.toUpperCase()
      for {
        generatedCode <- IO(generateCode)
        table <- tables.get(generatedCode)
        codeIO = table match {
          case Some(value) => IO.suspend(getNewGameCode)
          case None        => IO(generatedCode)
        }
        code <- codeIO
      } yield code
    }

    def getCookie(request: Request[IO], cookie: String) = request.cookies
      .find(_.name == cookie)
      .fold(IO.raiseError(new Exception(s"$cookie header not present")): IO[String])(c =>
        IO.pure(c.content)
      )

    def getTable(code: String) = for {
      tableOption <- tables.get(code)
      table <- tableOption.fold(
        IO.raiseError(new Exception(s"No table with code $code found")): IO[Table]
      )(IO.pure(_))
    } yield table

    implicit class ValueExtractor[T](either: Either[ErrorMessage, T]) {
      def io = either.fold(e => IO.raiseError(new Exception(e)): IO[T], IO.pure(_))
    }

    def updateTable(eitherTable: Either[ErrorMessage, Table], code: String) =
      eitherTable.fold(m => IO.raiseError(new Exception(m)), tables.put(code, _))

    def seatPlayer(table: Table, id: String, name: String, code: String) =
      updateTable(table.seatPlayer(name, id), code)

    def makeChoice(table: Table, id: String, choice: String, code: String): IO[Table] =
      updateTable(table.makeGameChoice(id, choice), code)

    def playCard(table: Table, id: String, card: String, code: String): IO[Table] =
      updateTable(table.playCard(id, card), code)

    def stashCards(table: Table, id: String, cards: String, code: String): IO[Table] =
      updateTable(table.stashCards(id, cards), code)

    def handleErrors(response: IO[Response[IO]]) =
      response.handleErrorWith(e => BadRequest(e.getMessage()))

    val helloRoute = {
      HttpRoutes.of[IO] { case GET -> Root =>
        Ok(s"Start a new game by sending a POST request to /new")
      }
    }

    val gameRoutes = {
      import io.circe.generic.auto._
      import io.circe.syntax._
      import org.http4s.circe.CirceEntityCodec._

      HttpRoutes.of[IO] {

        case POST -> Root / "new" => // could combine with functionality to register the invoker (of course need to privde name in req), include uuid in response
          for {
            code <- getNewGameCode
            text =
              s"Game table code: $code, proceed registering 3 players for the game, by sending " +
                "POST requests to /register with JSON with fields code and name"
            response <- tables.put(code, Table.empty) *> Ok(text)
          } yield response

        case req @ POST -> Root / "register" =>
          handleErrors(for {
            reg <- req.as[Registration]
            val Registration(code, name) = reg
            table <- getTable(code)
            // id <- IO(UUID.randomUUID().toString())
            id <- IO(Random.between(0, 1000).toString()) // for ease while developing
            table <- seatPlayer(table, id, name, code)
            text <- {
              val playersNeeded = table.playersNeeded

              // This logic could be moved to Table to return this info
              val infoIO =
                if (playersNeeded > 0)
                  IO.pure(s". Waiting for ${playersNeeded} more player(s).")
                else
                  table.whoseTurn.io.map(player =>
                    s". The game can begin. It's ${player.name}'s turn to make a game choice."
                  )
              infoIO.map(info => s"Hello, $name, you are registered.$info")
            }
            response <- Ok(text).map(_.addCookie("uuid", id).addCookie("code", code))
          } yield response)

        case req @ POST -> Root / "choice" / choice =>
          handleErrors(for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            table <- makeChoice(table, id, choice, code)
            game <- table.getGame.io
            text <- game match {
              case None =>
                table.whoMakesGameChoice.io.map(next =>
                  s"You passed, it's now ${next.name}'s turn to decide."
                )
              case Some(gameType) =>
                // this logic could be moved to Table to return such info
                val soloInfoIO = if (table.roundHasSoloPlayer) for {
                  solo <- table.soloPlayer.io
                  opponents <- table.getSoloPlayersOpponents.io
                } yield s" ${solo.name} will play solo against ${opponents.map(_.name).mkString(" and ")}."
                else IO.pure("")
                for {
                  info <- soloInfoIO
                  whoseTurn <- table.whoseTurn.io
                } yield s"The game type will be $gameType.$info It's now ${whoseTurn.name}'s turn to play a card."
            }
            response <- Ok(text)
          } yield response)

        //whose turn either to place a card or to make a game choice
        case req @ GET -> Root / "turn" =>
          handleErrors(for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            game <- table.getGame.io

            // this  logic could be moved to Table to return a 'status' String
            whoNeedsToAct <-
              if (game.isEmpty) table.whoMakesGameChoice.io
              else if (table.soloNeedsToStash) table.soloPlayer.io
              else table.whoseTurn.io
            text = "It's " + (if (whoNeedsToAct.id == id) "your"
                              else
                                whoNeedsToAct.name + "'s") + " turn to " + (if (game.isEmpty)
                                                                              "make a game choice"
                                                                            else if (
                                                                              table.soloNeedsToStash
                                                                            ) "stash two cards"
                                                                            else
                                                                              "play a card"
                                                                                + ".")
            response <- Ok(text)
          } yield response)

        // play a card
        case req @ POST -> Root / "turn" / card =>
          handleErrors(for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            table <- playCard(table, id, card, code)
            // continue here
            response <- Ok()
          } yield response)

        //stash cards
        case req @ POST -> Root / "stash" / cards =>
          handleErrors(for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            table <- stashCards(table, id, cards, code)
            //continue here
            response <- Ok()
          } yield response)

        // look at cards of the current trick, should include who played which card
        case req @ GET -> Root / "currentTrick" =>
          handleErrors(for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            // continue here
            response <- Ok()
          } yield response)

        case req @ GET -> Root / "getHandCards" =>
          handleErrors(for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            cards <- table.getPlayersCards(id).io
            response <- Ok(cards.mkString(", "))
          } yield response)
      }
    }

    helloRoute <+> gameRoutes
  }.orNotFound
}
