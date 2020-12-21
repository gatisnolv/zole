package com.gatis.bootcamp.project.zole

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.server.blaze.BlazeServerBuilder
import scala.concurrent.ExecutionContext
import com.gatis.bootcamp.project.zole.Routes.httpApp
import com.gatis.bootcamp.project.cache.ExpiringCache.Cache
import scala.concurrent.duration._

object ZoleServer extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = for {

    /*  The cache used is almost exactly the same as the one done for the shared state homework.
    I started to use this cache as a way to store the application state
    with the intention to later switch this out for something more persistent
    (a database). Due to time constraints I did not get around to that.
     */

    cache <- Cache.of[IO, String, Table](1.hour, 10.minutes)
    _ <- BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp(cache))
      .serve
      .compile
      .drain
  } yield ExitCode.Success
}
