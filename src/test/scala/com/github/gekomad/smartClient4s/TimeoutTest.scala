package com.github.gekomad.smartClient4s

import cats.effect.IO
import com.github.gekomad.smartClient4s.model.{BasicToken, PropertiesSmartClient4s, UriAndOpt}
import com.github.gekomad.smartClient4s.util.TestUtil.PropertiesSmartClient4sIT
import com.github.gekomad.smartClient4s.util.{Retry, TestStartServer}
import com.github.gekomad.smartClient4s.model.FEcontext
import io.circe.Json
import munit.CatsEffectSuite
import org.http4s.Method.GET
import org.http4s.implicits.uri
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration.*

class TimeoutTest extends CatsEffectSuite {
  private given Retry             = Retry(1, 1.second)
  private given Option[FEcontext] = None
  override def beforeAll(): Unit  = TestStartServer.startServer

  implicit val logger: Logger[IO]       = Slf4jLogger.getLoggerFromName[IO]("test1")
  private given PropertiesSmartClient4s = PropertiesSmartClient4sIT

  test("timeout test") {
    for {
      client <- HttpClientProvider.httpClientsResource("client1", proxy = None)
      (body, status, _) <- client.call[Unit, Json](
        UriAndOpt(uri"http://127.0.0.1:9111/api/delay/3", basicToken = Some(BasicToken.apply("admin", "pass123"))),
        GET,
        timeout = 1.second
      )
    } yield {
      assert(body.spaces2.contains("TimeoutException"))
      assert(status.code == 500)
    }
  }

  test("timeout2 test") {
    for {
      client <- HttpClientProvider.httpClientsResource("client1", proxy = None)
      (_, status, _) <- client.call[Unit, Json](
        UriAndOpt(uri"http://127.0.0.1:9111/api/delay/3", basicToken = Some(BasicToken.apply("admin", "pass123"))),
        GET,
        timeout = 11.second
      )
    } yield {
      assert(status.isSuccess)
    }
  }

}
