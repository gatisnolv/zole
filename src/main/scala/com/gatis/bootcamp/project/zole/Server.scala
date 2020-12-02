package com.gatis.bootcamp.project.zole

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.server.blaze.BlazeServerBuilder
import scala.concurrent.ExecutionContext
import com.gatis.bootcamp.project.zole.Routes.httpApp

object ZoleHttpServer extends IOApp {
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
