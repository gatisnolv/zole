package com.gatis.bootcamp.project.zole

// import cats.effect.{Blocker, IO}
import cats.effect.{IO}
import org.http4s.dsl.io._
import org.http4s._
import cats.syntax.all._
import org.http4s.implicits._
import scala.util.Random
import java.util.UUID

case class CardR(rank: String, suit: String)

case class Registration(code: String, name: String)

object Routes {
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
          response <- Ok(
            s"Game table code: $code, proceed registering 3 players for the game, by sending POST requests to /register with JSON with fields code and name"
          )
        } yield response

      case req @ POST -> Root / "register" =>
        for {
          reg <- req.as[Registration]
          // TODO actual registration
          name = "John"
          nameUsed = false
          playersNeeded = 2
          id <- IO(UUID.randomUUID().toString())
          response <-
            Ok(
              if (nameUsed)
                s"There is already someone at the table with name ${reg.name}, please use a different name"
              else
                s"Hello, ${reg.name}, you are registered" +
                  (if (playersNeeded > 0)
                     s", waiting for ${playersNeeded} more player${if (playersNeeded > 1) "s"}."
                   else s". The game can begin. It's ${name} turn to make a choice how to play.")
            ).map(r => if (nameUsed) r else r.addCookie("uuid", id))
          //could already deal cards (i.e. the cards are pre dealt), but not suitable for black zole, so rather have separate getCards endpoint
        } yield response

      //following registration next requests can be identified with uuid
      case req @ GET -> Root / "whoseTurn" => // would be nice to use for both turn as well as turn to make a choice how to play
        (for {
          id <- getId(req)
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

      case req @ POST -> Root / "choice" / choice =>
        for {
          id <- getId(req)
          shouldChoose = true
          whoseTurn = "John"
          response <- Ok(
            if (shouldChoose) s"You chose ${choice}, it's now $whoseTurn's turn."
            else s"It's not your turn to make a game choice, it's $whoseTurn's."
          )
        } yield response

      case req @ POST -> Root / "pickUpCards" =>
        (for {
          // could define specific exceptions
          id <- getId(req)
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

  def getNewGameCode = {
    // 3 letter code supports (26^3) 17576 games
    // add check for uniqueness
    IO(Random.alphanumeric.filter(_.isLetter).take(3).mkString.toUpperCase())
  }

  def getId(request: Request[IO]) = request.cookies
    .find(_.name == "uuid")
    .fold(IO.raiseError(new RuntimeException("uuid header not present")): IO[String])(c =>
      IO.pure(c.content)
    )

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

  private[zole] val httpApp = { helloRoute <+> cardRoute <+> gameRoutes }.orNotFound
}
