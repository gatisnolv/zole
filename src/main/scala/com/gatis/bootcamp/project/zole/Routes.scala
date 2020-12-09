package com.gatis.bootcamp.project.zole

// import cats.effect.{Blocker, IO}
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

// think about naming
case class Choice(choice: String)

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

    def updateTable(eitherTable: Either[ErrorMessage, Table], code: String) =
      eitherTable.fold(m => IO.raiseError(new Exception(m)), tables.put(code, _))

    def seatPlayer(table: Table, name: String, id: String, code: String) = for {
      table <- updateTable(table.seatPlayer(name, id), code)
    } yield table

    def handleErrors(response: IO[Response[IO]]) =
      response.handleErrorWith(e => BadRequest(e.getMessage()))

    def makeChoice(table: Table, id: String, choice: String, code: String): IO[Table] = for {
      // TODO continue here
      table <- updateTable(table.makeGameChoice(id, choice), code)
    } yield table

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
            id <- IO(UUID.randomUUID().toString())
            table <- seatPlayer(table, id, name, code)
            response <- {
              val playersNeeded = table.playersNeeded
              val text = s"Hello, $name, you are registered ${if (playersNeeded > 0)
                s", waiting for ${playersNeeded} more player(s)."
              else
                // .last can throw if the list is empty, but is that a problem for this?
                s". The game can begin. It's ${table.players.last.name}'s turn to choose a game type."}"
              //could already deal (hand out) cards, though not suitable for black zole, so rather use separate getCards endpoint after all register
              Ok(text).map(_.addCookie("uuid", id).addCookie("code", code))
            }
          } yield response)
        //add handling for nonexistent table with 4xx - for most requests, since they rely on getCookie

        case req @ POST -> Root / "choice" / choice =>
          handleErrors(for {
            choice <- req.as[Choice]
            id <- getCookie(req, "uuid")
            code <- getCookie(req, "code")
            table <- getTable(code)
            table <- makeChoice(table, id, choice.choice, code)
            // TODO continue from here
            shouldChoose = true
            whoseTurn = "John"
            response <- Ok(
              if (shouldChoose) s"You chose ${choice}, it's now $whoseTurn's turn."
              else s"It's not your turn to make a game choice, it's $whoseTurn's."
            )
          } yield response)

        case req @ POST -> Root / "getCards" => { ??? }

        //following registration next requests can be identified with uuid, table code
        case req @ GET -> Root / "whoseTurn" => // would be nice to use for both turn as well as turn to make a choice how to play
          (for {
            id <- getCookie(req, "uuid")
            name = "John"
            tableReady = true
            shouldChoose = true
            playersNeeded = 2
            response <- Ok(
              if (tableReady)
                if (shouldChoose)
                  "It's your turn to make a choice how to play"
                else
                  s"It's $name's turn"
              else s"Still waiting for ${playersNeeded} player${if (playersNeeded > 1) "s"} to join"
            )
          } yield response).handleErrorWith(e => BadRequest(e.getMessage()))

        case req @ POST -> Root / "pickUpCards" =>
          (for {
            // could define specific exceptions
            id <- getCookie(req, "uuid")
            tableReady = true
            name = "John"
            response <- Ok(
              if (tableReady)
                //mark cards as picked up for player
                s"Your cards are in the body JSON, it is $name's turn"
              else
                s"Waiting for more players to join the table"
            ) // add cards to body if table ready
          } yield response).handleErrorWith(e => BadRequest(e.getMessage()))

      }
    }

    //experiment
    val cardRoute = {
      import io.circe.generic.auto._
      import io.circe.syntax._
      import org.http4s.circe.CirceEntityCodec._
      HttpRoutes.of[IO] { case req @ POST -> Root / "card" =>
        for {
          card <- req.as[CardR]
          inpuCard = Card.of(card.rank + card.suit)
          response <- Ok(inpuCard.fold(e => e, c => c).toString)
        } yield response
      }
    }

    helloRoute <+> cardRoute <+> gameRoutes
  }.orNotFound
}
