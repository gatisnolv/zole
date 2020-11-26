package com.gatis.bootcamp.project.zole
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import org.http4s.server.blaze.BlazeServerBuilder
import scala.concurrent.ExecutionContext
import org.http4s.dsl.io._
import org.http4s._
import cats.syntax.all._
import org.http4s.implicits._

case class CardR(rank: String, suit: String)
object ZoleHttpServer extends IOApp {

  val helloRoute = {
    HttpRoutes.of[IO] { case GET -> Root =>
      Ok(s"Hi, there!")
    }
  }

  val cardRoute = {
    import io.circe.generic.auto._
    import io.circe.syntax._
    import org.http4s.circe.CirceEntityCodec._
    HttpRoutes.of[IO] { case req @ POST -> Root / "card" =>
      req.as[CardR].flatMap { card =>
        val inputcard = Card.of(card.rank + card.suit)
        Ok(inputcard.fold(e => e, c => c).toString)
      }
    }
  }

  private[zole] val httpApp = { helloRoute <+> cardRoute }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = BlazeServerBuilder[IO](
    ExecutionContext.global
  )
    .bindHttp(port = 9001, host = "localhost")
    .withHttpApp(httpApp)
    .serve
    .compile
    .drain
    .as(ExitCode.Success)
}
