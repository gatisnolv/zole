package com.gatis.bootcamp.project.zole

import cats.effect.IO
import org.http4s.dsl.io._
import org.http4s._
import cats.syntax.all._
import org.http4s.implicits._
import scala.util.Random
import java.util.UUID
import com.gatis.bootcamp.project.cache.ExpiringCache.Cache

object Routes {

  case class Registration(code: String, name: String)

  def getCookie(request: Request[IO], cookie: String) = request.cookies
    .find(_.name == cookie)
    .fold(IO.raiseError(new Exception(s"$cookie header not present")): IO[String])(c =>
      IO.pure(c.content)
    )

  implicit class ValueExtractor[T](value: Either[ErrorMessage, T]) {
    def io = value.fold(e => IO.raiseError(new Exception(e)): IO[T], IO.pure(_))
  }

  implicit class ErrorHandler(response: IO[Response[IO]]) {
    // could have different behaviour based on the message, i.e. 4xx, 5xx for unexpected errors
    def handleErrors = response.handleErrorWith(e => BadRequest(e.getMessage()))
  }

  private[zole] def httpApp(tables: Cache[IO, String, Table]) = {

    // 3 letter code supports (26^3) 17576 games
    def getNewGameCode: IO[String] = {
      // check for uniqueness
      def generateCode = Random.alphanumeric.filter(_.isLetter).take(3).mkString.toUpperCase()
      for {
        generatedCode <- IO(generateCode)
        table <- tables.get(generatedCode)
        codeIO = table.fold(IO.pure(generatedCode))(_ => IO.suspend(getNewGameCode))
        code <- codeIO
      } yield code
    }

    def getTable(code: String) = for {
      tableOption <- tables.get(code)
      table <- tableOption.fold(
        IO.raiseError(new Exception(s"No table with code $code found")): IO[Table]
      )(IO.pure(_))
    } yield table

    implicit class PersistenceProvider(table: Either[ErrorMessage, Table]) {
      def save(code: String): IO[Table] =
        table.fold(m => IO.raiseError(new Exception(m)), tables.put(code, _))
    }

    val helloRoute = {
      HttpRoutes.of[IO] { case GET -> Root =>
        Ok(s"Start a new game by sending a POST request to /new")
      }
    }

    val gameRoutes = {
      import io.circe.generic.auto._
      // import io.circe.syntax._
      import org.http4s.circe.CirceEntityCodec._

      HttpRoutes.of[IO] {

        // could combine with functionality to register the invoker (of course need to privde name in req), include uuid in response
        case POST -> Root / "new" =>
          for {
            // code <- getNewGameCode
            // for ease while developing
            code <- IO.pure("AAA")
            text =
              s"Game table code: $code, proceed registering 3 players for the game, by sending " +
                "POST requests to /register with JSON in body with fields code and name"
            response <- tables.put(code, Table.empty) *> Ok(text)
          } yield response

        case req @ POST -> Root / "register" =>
          (for {
            reg <- req.as[Registration]
            val Registration(code, name) = reg
            table <- getTable(code)
            // id <- IO(UUID.randomUUID().toString())
            // for ease while developing
            id <- IO(
              (table.players.foldLeft(0)((acc, el) => Math.max(acc, el.id.toInt)) + 1).toString
            )
            table <- table.seatPlayer(name, id).save(code)
            info = table.statusInfo(id)
            text = s"Hello, $name, you are registered. " +
              (if (table.morePlayersNeeded) "" else "The game can begin. ") + info
            response <- Ok(text).map(_.addCookie("uuid", id).addCookie("code", code))
          } yield response).handleErrors

        case req @ GET -> Root / "getHandCards" =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            hand <- table.playersHand(id).io
            response <- Ok(hand.mkString(", "))
          } yield response).handleErrors

        case req @ POST -> Root / "choice" / choice =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            game <- table.getGame.io
            table <- table.makeGameChoice(id, choice).save(code)
            text <- table.gameChoiceInfo(game).io
            response <- Ok(text)
          } yield response).handleErrors

        case req @ POST -> Root / "stash" / cards =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            table <- table.stashCards(id, cards).save(code)
            hand <- table.playersHand(id).io
            response <- Ok(hand.mkString(", "))
          } yield response).handleErrors

        case req @ GET -> Root / "turnOrder" =>
          (for {
            code <- getCookie(req, "code")
            table <- getTable(code)
            info <- table.turnOrderInfo.io
            response <- Ok(info)
          } yield response).handleErrors

        case req @ POST -> Root / "play" / card =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            table <- table.playCard(id, card).save(code)
            // TODO could return hand cards post playing the card
            // TODO continue here
            response <- Ok()
          } yield response).handleErrors

        case req @ GET -> Root / "turn" =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            info = table.statusInfo(id)
            response <- Ok(info)
          } yield response).handleErrors

        // look at cards of the current trick, should include who played which card
        case req @ GET -> Root / "currentTrick" =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            // TODO continue here
            response <- Ok()
          } yield response).handleErrors

        // TODO add endpoint for looking at last trick

        // TODO endpoint for round points

        // TODO endpoint for scores

        case req @ POST -> Root / "nextRound" =>
          (for {
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            table <- table.nextRound.save(code)
            info = table.statusInfo(id)
            text = "The next round can begin. " + info
            // TODO continue here
            response <- Ok()
          } yield response).handleErrors
      }
    }

    helloRoute <+> gameRoutes
  }.orNotFound
}
