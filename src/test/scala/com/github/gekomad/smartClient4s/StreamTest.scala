package com.github.gekomad.smartClient4s

import cats.effect.IO
import com.github.gekomad.smartClient4s.model.{BasicToken, PropertiesSmartClient4s, UriAndOpt}
import com.github.gekomad.smartClient4s.util.TestStartServer
import com.github.gekomad.smartClient4s.util.TestUtil.PropertiesSmartClient4sIT
import munit.CatsEffectSuite
import org.http4s.Method.GET
import org.http4s.implicits.uri
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration.*

class StreamTest extends CatsEffectSuite {

  override def beforeAll(): Unit = TestStartServer.startServer

  implicit val logger: Logger[IO]       = Slf4jLogger.getLoggerFromName[IO]("test1")
  private given PropertiesSmartClient4s = PropertiesSmartClient4sIT

  test("stream test") {
    import fs2.text
    case class CSVrecord(field1: Int, field2: String)
    val timeout   = 1.seconds
    val separator = ","
    for {
      client <- HttpClientProvider.httpClientsResource(proxy = None)
      (time, (status, value)) <- client
        .resource(
          UriAndOpt(uri"http://127.0.0.1:9111/api/csv", basicToken = Some(BasicToken.apply("admin", "pass123"))),
          GET
        )
        .use { response =>
          response.body
            .through(text.utf8.decode)
            .through(text.lines)
            .filter(line => line.startsWith("1") && line.contains(separator))
            .map { line =>
              val cols = line.split(separator)
              CSVrecord(
                field1 = cols(0).toIntOption.getOrElse(0),
                field2 = cols(1)
              )
            }
            .compile
            .toVector
            .map(records => (response.status, records))
        }
        .timeout(timeout)
        .timed
    } yield {
      println(s"time: $time")
      assert(value == Vector(CSVrecord(1, "aaa"), CSVrecord(1, "ccc")))
      assert(status.code == 200)
    }
  }
}
