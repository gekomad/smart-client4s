package com.github.gekomad.smartClient4s.util

import cats.data.*
import cats.effect.*
import org.http4s.*
import org.http4s.Header.ToRaw
import org.http4s.dsl.io.*
import org.http4s.headers.{Authorization, `WWW-Authenticate`}
import org.http4s.server.*
import org.typelevel.ci.CIString

type BasicAuthUser = String
case class BasicAuthentication(cred: Map[String, String]) {

  private def credentials(user: String, pass: String): Boolean = cred.get(user).contains(pass)

  private val authUserEither: Kleisli[IO, Request[IO], Either[String, String]] = Kleisli { req =>
    req.headers.get[Authorization] match {
      case Some(Authorization(BasicCredentials(creds))) =>
        if (credentials(creds._1, creds._2)) IO(Right(creds._1))
        else IO(Left("Wrong credentials"))
      case Some(_) =>
        IO(Left("No basic credentials"))
      case None =>
        IO(Left("Unauthorized"))
    }
  }

  private val onFailure: AuthedRoutes[BasicAuthUser, IO] = {
    val askAuthentication: IO[Response[IO]] =
      org.http4s.dsl.io.Unauthorized.apply(
        `WWW-Authenticate`(NonEmptyList.of(Challenge(scheme = "Bearer", realm = "Please enter a valid API key"))),
        ToRaw.rawToRaw(Header.Raw(CIString("WWW-Authenticate"), """Basic realm="My Realm""""))
      )

    Kleisli(_ => OptionT.liftF(askAuthentication))
  }

  val authMiddleware: AuthMiddleware[IO, String] = AuthMiddleware(authUserEither, onFailure)

}
