package com.gatis.bootcamp.project.cache

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}
import cats.implicits._

import scala.concurrent.duration._

object ExpiringCache {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[V]
  }

  class RefCache[F[_]: Clock: Monad, K, V](state: Ref[F, Map[K, (Long, V)]])
      extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = for {
      map <- state.get
    } yield map.get(key).map { case (_, value) => value }

    def put(key: K, value: V): F[V] = for {
      time <- Clock[F].realTime(MILLISECONDS)
      _ <- state.update(_.updated(key, (time, value)))
    } yield value

  }

  object Cache {
    def of[F[_]: Clock, K, V](
      expiresIn: FiniteDuration,
      checkOnExpirationsEvery: FiniteDuration
    )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {

      def cleanExpiredEntries(state: Ref[F, Map[K, (Long, V)]]) = for {
        time <- Clock[F].realTime(MILLISECONDS)
        _ <- state.update(_.filter { case (_, (timeCached, _)) =>
          time - timeCached < expiresIn.toMillis
        })
      } yield ()

      for {
        state <- Ref.of[F, Map[K, (Long, V)]](Map.empty)
        _ <- C.start((T.sleep(checkOnExpirationsEvery) *> cleanExpiredEntries(state)).foreverM.void)
      } yield new RefCache(state)
    }
  }

}
