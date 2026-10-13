package com.github.gekomad.smartClient4s

import cats.effect.IO
import com.github.gekomad.smartClient4s.HttpHeader.textPlainCT
import com.github.gekomad.smartClient4s.model.{BasicToken, FEcontext, PropertiesSmartClient4s, UriAndOpt}
import com.github.gekomad.smartClient4s.util.TestUtil.{PropertiesSmartClient4sIT, TestMyData}
import com.github.gekomad.smartClient4s.util.{CallCodec, Retry, TestStartServer}
import io.circe.generic.auto.*
import io.circe.parser.parse
import munit.CatsEffectSuite
import org.http4s.Method.GET
import org.http4s.Status
import org.http4s.headers.`Content-Type`
import org.http4s.implicits.uri
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration.*

class CustomTest extends CatsEffectSuite {
  private given Retry             = Retry(1, 1.second)
  private given Option[FEcontext] = None
  override def beforeAll(): Unit  = TestStartServer.startServer

  implicit val logger: Logger[IO]       = Slf4jLogger.getLoggerFromName[IO]("test1")
  private given PropertiesSmartClient4s = PropertiesSmartClient4sIT

  test("custom test") {

    // Unit -> TestMyData
    given CallCodec[Unit, TestMyData] with {
      def encodeBody(in: Option[Unit]): Option[String] = None

      def contentType: `Content-Type` = textPlainCT

      def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit
        feContext: Option[FEcontext]
      ): IO[(TestMyData, Status, Boolean)] = {
        val a = parse(payload).getOrElse(???)
        val p = a.as[TestMyData].getOrElse(???)
        IO(p, status, fromCache)
      }
    }

    for {
      client <- HttpClientProvider.httpClientsResource(proxy = None)
      (body, status, _) <- client.call[Unit, TestMyData](
        UriAndOpt(uri"http://127.0.0.1:9111/api/my_class", basicToken = Some(BasicToken.apply("admin", "pass123"))),
        GET,
        timeout = 1.second
      )
    } yield {
      assert(body == TestMyData("Bob", 12))
      assert(status.code == 200)
    }
  }
}
