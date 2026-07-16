package com.github.gekomad.smartClient4s.util

import com.comcast.ip4s.Port
import com.github.gekomad.smartClient4s.model.LogDirection
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID
import org.http4s.{Header, Status, Uri}
import org.typelevel.ci.CIString
import scala.util.{Failure, Success, Try}
import com.github.gekomad.smartClient4s.model.FEcontext

object DecoderCirce {

  import io.circe.*
  private given Option[FEcontext]   = None
  private implicit val log: Plogger = Plogger(this.getClass)
  given Encoder[Status]             = Encoder[Int].contramap(_.code)

  given Decoder[Status] = Decoder[Int].emap { code =>
    Status.fromInt(code) match {
      case Right(status) => Right(status)
      case Left(_)       => Left(s"Invalid status code: $code")
    }
  }

  private val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  given Encoder[LocalDateTime] = Encoder.encodeString.contramap[LocalDateTime](formatter.format)

  given Decoder[LocalDateTime] = Decoder.decodeString.emap { str =>
    Try(LocalDateTime.parse(str, formatter)) match {
      case Success(value) => Right(value)
      case Failure(_) =>
        Try(LocalDateTime.parse("1970-01-02 03:04:05", formatter)) match {
          case Failure(e) =>
            log.error(s"Error in date parsing $str: ${e.getMessage} force: 1970-01-02 03:04:05")
            Left(s"Error in date parsing $str: ${e.getMessage}")
          case Success(value1) => Right(value1)
        }
    }
  }

  given Encoder[LogDirection] = Encoder[String].contramap(_.direction)

  given Decoder[LogDirection] = Decoder.decodeString.emap { str =>
    LogDirection.values.find(_.direction == str).toRight(s"unknown LogDirection $str")
  }

  given Decoder[UUID] =
    Decoder[String].emap { a =>
      Try(UUID.fromString(a)).toOption match {
        case None        => Left(s"$a is not a valid UUID")
        case Some(value) => Right(value)
      }
    }

  given Encoder[UUID] = Encoder[String].contramap(_.toString)
  given Decoder[Uri] =
    Decoder[String].emap { a =>
      Uri.fromString(a) match {
        case Left(value)  => Left(value.message)
        case Right(value) => Right(value)
      }
    }

  given Encoder[Uri] = Encoder[String].contramap(_.toString)

  given Decoder[Port] =
    Decoder[Int].emap { a =>
      Port.fromInt(a) match {
        case None    => Left(s"$a is not a valid port")
        case Some(p) => Right(p)
      }
    }

  given Decoder[Header.Raw] = (c: HCursor) =>
    for {
      name  <- c.downField("name").as[String]
      value <- c.downField("value").as[String]
    } yield Header.Raw.apply(CIString(name), value)

}
