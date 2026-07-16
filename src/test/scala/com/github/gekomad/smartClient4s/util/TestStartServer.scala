package com.github.gekomad.smartClient4s.util

import cats.effect.*
import cats.effect.unsafe.implicits.global
import com.comcast.ip4s.{Port, host, port}
import com.github.gekomad.smartClient4s.util.TestUtil.MyData
import io.circe.Json
import io.circe.generic.auto
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.{Router, Server}
import org.typelevel.log4cats.slf4j.Slf4jFactory

object TestStartServer {
  private val credentialsAdmin: Map[BasicAuthUser, BasicAuthUser] = Map(
    "admin" -> "pass123"
  )

  private def adminRoutes: AuthedRoutes[BasicAuthUser, IO] = {
    AuthedRoutes.of {
      case GET -> Root / "api" / "delay" / i as _ =>
        Thread.sleep(i.toInt * 1000)
        Ok()
      case req @ POST -> Root / "api" / "get_long_service" as _ =>
        req.req.as[Json].flatMap { _ =>
          Thread.sleep(2000)
          Ok(MyData("Maria", 44).asJson)
        }
    }
  }

  private def serviceRouter: HttpRoutes[IO] = Router(
    "/" -> BasicAuthentication(credentialsAdmin).authMiddleware(adminRoutes)
  )
  private given Slf4jFactory[IO] = Slf4jFactory.create[IO]
  lazy val startServer: Unit = {

    val httpApp: HttpRoutes[IO]   = serviceRouter
    val finalHttpApp: HttpApp[IO] = org.http4s.server.middleware.Logger.httpApp(logHeaders = true, logBody = true)(httpApp.orNotFound)
    val r: Resource[IO, Server] = EmberServerBuilder
      .default[IO]
      .withHost(host"0.0.0.0")
      .withPort(port"9111")
      .withHttpApp(finalHttpApp)
      .build

    r.useForever
      .handleErrorWith { e =>
        IO.raiseError(new NotImplementedError(s"error starting server ${e.getMessage}"))
      }
      .unsafeRunAndForget()
  }
}
