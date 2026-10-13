package com.github.gekomad.smartClient4s.util

import cats.effect.IO
import com.github.gekomad.smartClient4s.HttpHeader.{applicationJsonCT, textPlainCT}
import com.github.gekomad.smartClient4s.PayloadError
import com.github.gekomad.smartClient4s.model.{FEcontext, UriAndOpt}
import org.http4s.Status.InternalServerError
import org.http4s.headers.`Content-Type`
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.{Encoder, Json}
import org.http4s.Status

// typeclass to trasform input/output
trait CallCodec[In, Out] {
  def encodeBody(in: Option[In]): Option[String]

  def contentType: `Content-Type`

  def decodeResponse(
    payload: String,
    status: Status,
    fromCache: Boolean,
    uri: UriAndOpt
  )(implicit feContext: Option[FEcontext]): IO[(Out, Status, Boolean)]
}

object CallCodec {
  private given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger: SelfAwareStructuredLogger[IO] = LoggerFactory[IO].getLogger
  private def toJson(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt): IO[(Json, Status, Boolean)] = {
    if (payload.isBlank) IO((Json.Null, status, fromCache))
    else
      io.circe.parser.parse(payload) match {
        case Left(_) =>
          val st = if (status.isSuccess) InternalServerError else status
          logger.error(s"status: ${status.code} uri: $uri decode Json error: $payload") *>
            IO((PayloadError(uri = uri.uri.toString, description = payload).asJson, st, fromCache))
        case Right(v) => IO((v, status, fromCache))
      }
  }

  // Json -> Json
  given CallCodec[Json, Json] with {
    def encodeBody(in: Option[Json]): Option[String] = in.map(_.spaces2)

    def contentType: `Content-Type` = applicationJsonCT

    def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit
      feContext: Option[FEcontext]
    ) = toJson(payload, status, fromCache, uri)
  }

  // Json -> Unit
  given CallCodec[Json, Unit] with {
    def encodeBody(in: Option[Json]): Option[String] = in.map(_.spaces2)

    def contentType: `Content-Type` = applicationJsonCT

    def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit
      feContext: Option[FEcontext]
    ) =
      IO(((), status, fromCache))
  }

  // Json -> String
  given CallCodec[Json, String] with {
    def encodeBody(in: Option[Json]): Option[String] = in.map(_.spaces2)

    def contentType: `Content-Type` = applicationJsonCT

    def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit feContext: Option[FEcontext]) =
      IO((payload, status, fromCache))
  }

  // String -> String
  given CallCodec[String, String] with {
    def encodeBody(in: Option[String]): Option[String] = in

    def contentType: `Content-Type` = textPlainCT

    def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit feContext: Option[FEcontext]) =
      IO((payload, status, fromCache))
  }

  // String -> Json
  given CallCodec[String, Json] with {
    def encodeBody(in: Option[String]): Option[String] = in

    def contentType: `Content-Type` = textPlainCT

    def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit
      feContext: Option[FEcontext]
    ): IO[(Json, Status, Boolean)] =
      toJson(payload, status, fromCache, uri)
  }

  // String -> Unit
  given CallCodec[String, Unit] with {
    def encodeBody(in: Option[String]): Option[String] = in

    def contentType: `Content-Type` = textPlainCT

    def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit feContext: Option[FEcontext]) = IO(
      ((), status, fromCache)
    )
  }

  // Unit -> Json
  given CallCodec[Unit, Json] with {
    def encodeBody(in: Option[Unit]): Option[String] = None

    def contentType: `Content-Type` = textPlainCT

    def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit
      feContext: Option[FEcontext]
    ): IO[(Json, Status, Boolean)] =
      toJson(payload, status, fromCache, uri)
  }

  // Unit -> Unit
  given CallCodec[Unit, Unit] with {
    def encodeBody(in: Option[Unit]): Option[String] = None

    def contentType: `Content-Type` = textPlainCT

    def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit feContext: Option[FEcontext]) =
      IO(((), status, fromCache))
  }

  // Unit -> String
  given CallCodec[Unit, String] with {
    def encodeBody(in: Option[Unit]): Option[String] = None

    def contentType: `Content-Type` = textPlainCT

    def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit feContext: Option[FEcontext]) = IO(
      (payload, status, fromCache)
    )
  }
}
