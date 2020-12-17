package com.gatis.bootcamp.project.zole

import cats.effect.IO
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

    implicit class ValueExtractor[T](value: Either[ErrorMessage, T]) {
      def io = value.fold(e => IO.raiseError(new Exception(e)): IO[T], IO.pure(_))
    }

    implicit class PersistenceProvider(table: Either[ErrorMessage, Table]) {
      def save(code: String): IO[Table] =
        table.fold(m => IO.raiseError(new Exception(m)), tables.put(code, _))
    }

    implicit class ErrorHandler(response: IO[Response[IO]]) {
      //could have different behaviour based on the message, i.e. 4xx, 5xx for unexpected errors
      def handleErrors = response.handleErrorWith(e => BadRequest(e.getMessage()))
    }

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
            // code <- getNewGameCode
            code <- IO.pure("AAA") // for ease while developing
            text =
              s"Game table code: $code, proceed registering 3 players for the game, by sending " +
                "POST requests to /register with JSON with fields code and name"
            response <- tables.put(code, Table.empty) *> Ok(text)
          } yield response

        case req @ POST -> Root / "register" =>
          (for {
            reg <- req.as[Registration]
            val Registration(code, name) = reg
            table <- getTable(code)
            // id <- IO(UUID.randomUUID().toString())
            id <- IO(
              (table.players.foldLeft(0)((acc, el) => Math.max(acc, el.id.toInt)) + 1).toString
            ) // for ease while developing
            table <- table.seatPlayer(name, id).save(code)
            playersNeeded = table.playersNeeded
            info <- table.statusInfo(id).io
            text = s"Hello, $name, you are registered. " +
              (if (playersNeeded == 0) "The game can begin. " else "") + info
            response <- Ok(text).map(_.addCookie("uuid", id).addCookie("code", code))
          } yield response).handleErrors

        case req @ GET -> Root / "getHandCards" =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            cards <- table.playersCards(id).io
            response <- Ok(cards.mkString(", "))
          } yield response).handleErrors

        case req @ POST -> Root / "choice" / choice =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            table <- table.makeGameChoice(id, choice).save(code)
            game <- table.getGame.io
            text <- game match {
              case None =>
                table.whoMakesGameChoice.io.map(next =>
                  s"You passed, it's now $next's turn to decide."
                )
              case Some(gameType) =>
                // this logic could be moved to Table to return such info
                val soloInfoIO = if (table.roundHasSoloPlayer) for {
                  solo <- table.soloPlayer.io
                  opponents <- table.getSoloPlayersOpponents.io
                } yield s" $solo will play solo against ${opponents.map(_.name).mkString(" and ")}."
                else IO.pure("")
                for {
                  info <- soloInfoIO
                  whoseTurn <- table.whoseTurn.io
                } yield s"The game type will be $gameType.$info It's now $whoseTurn's turn to play a card."
            }
            response <- Ok(text)
          } yield response).handleErrors

        //stash cards
        case req @ POST -> Root / "stash" / cards =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            table <- table.stashCards(id, cards).save(code)
            cards <- table.playersCards(id).io
            response <- Ok(cards.mkString(", "))
          } yield response).handleErrors

        // play a card
        case req @ POST -> Root / "turn" / card =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            table <- table.playCard(id, card).save(code)
            //could return hand cards post playing the card
            // continue here
            response <- Ok()
          } yield response).handleErrors

        //whose turn either to place a card or to make a game choice
        case req @ GET -> Root / "turn" =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            game <- table.getGame.io
            info <- table.statusInfo(id).io
            response <- Ok(info)
          } yield response).handleErrors

        // look at cards of the current trick, should include who played which card
        case req @ GET -> Root / "currentTrick" =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            // continue here
            response <- Ok()
          } yield response).handleErrors

      }
    }

    helloRoute <+> gameRoutes
  }.orNotFound
}
