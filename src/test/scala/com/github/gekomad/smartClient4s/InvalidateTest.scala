package com.github.gekomad.smartClient4s

import cats.effect.IO
import com.github.gekomad.smartClient4s.model.PropertiesSmartClient4s
import com.github.gekomad.smartClient4s.util.TestUtil.{MyData, PropertiesSmartClient4sIT}
import com.github.gekomad.smartClient4s.util.{Retry, TestHelper, TestStartServer}
import io.circe.Json
import io.circe.generic.auto.*
import munit.CatsEffectSuite
import org.http4s.Method.POST
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

class InvalidateTest extends CatsEffectSuite {
  override def beforeAll(): Unit = TestStartServer.startServer

  implicit val logger: Logger[IO]       = Slf4jLogger.getLoggerFromName[IO]("test1")
  private given PropertiesSmartClient4s = PropertiesSmartClient4sIT
  private given Retry                   = Retry(1, 1.second)

  test("Invalidate cache") {
    HttpClientProvider.httpClientsResource(proxy = None).flatMap { client =>
      for {
        // 1. First call: Not in cache
        (payload, status, fromCache, sha1, tempoNoCache) <- client
          .callWithInfo[Json, Json](
            TestHelper.fetchDataUrl,
            POST,
            body = Some(TestHelper.fetchDataPayload("key_cached")),
            ttlCache = Some(2.hours)
          )
        _ = assert(payload.as[MyData].isRight)
        _ = assert(status.isSuccess)
        _ = assert(sha1.isDefined)
        _ = assert(!fromCache)
        // 2. Second call: Uses the cache
        (payload, status, fromCache, sha2, tempoCache) <- client
          .callWithInfo[Json, Json](
            TestHelper.fetchDataUrl,
            POST,
            body = Some(TestHelper.fetchDataPayload("key_cached")),
            ttlCache = Some(2.hours)
          )

        _ = assert(payload.as[MyData].isRight)
        _ = assert(status.isSuccess)
        _ = assert(fromCache)
        _ = assert(sha1 == sha2)

        // 3. Invalidate the cache
        _ <- client.invalidate(sha1.getOrElse(???))(using None)

        // 4. Third call: Empty cache, must execute the real call
        (payload, status, fromCache, sha3, afterInvalidate) <- client
          .callWithInfo[Json, Json](
            TestHelper.fetchDataUrl,
            POST,
            body = Some(TestHelper.fetchDataPayload("key_cached")),
            ttlCache = Some(2.hours)
          )

        _ = assert(payload.as[MyData].isRight)
        _ = assert(status.isSuccess)
        _ = assert(!fromCache)
        _ = assert(sha1 == sha3)
        _ = assert(tempoNoCache >= 2.seconds, s"Expected >= 2s")
        _ = assert(tempoCache < 2.seconds, s"Expected < 2s")
        _ = assert(afterInvalidate >= 2.seconds, s"Expected >= 2s after invalidate")

      } yield ()
    }
  }
}
