package com.gatis.bootcamp.project.zole

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.server.blaze.BlazeServerBuilder
import scala.concurrent.ExecutionContext
import com.gatis.bootcamp.project.zole.Routes.httpApp
import com.gatis.bootcamp.project.cache.ExpiringCache.Cache
import scala.concurrent.duration._

object ZoleServer extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = for {

    // TODO choose duration that make sense
    cache <- Cache.of[IO, String, Table](10.minutes, 2.minutes)
    _ <- BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp(cache))
      .serve
      .compile
      .drain
  } yield ExitCode.Success
}
