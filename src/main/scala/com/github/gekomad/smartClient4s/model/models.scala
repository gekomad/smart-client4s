package com.github.gekomad.smartClient4s.model

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import com.comcast.ip4s.Port
import org.http4s.{Header, Status, Uri}

import java.time.LocalDateTime
import java.util.UUID

val statusOK: List[Status] = List(
  Status.Ok,
  Status.Accepted,
  Status.NoContent,
  Status.Created,
  Status.NonAuthoritativeInformation,
  Status.AlreadyReported,
  Status.IMUsed,
  Status.MultiStatus,
  Status.PartialContent,
  Status.ResetContent
)

case class ProxyUriPort(uri: Uri, port: Port)
case class LogData(kafkaLogUri: UriAndOpt, domain: String, sendToLogTimeout: FiniteDuration = 7.seconds, maxPayloadSize: Int = 200 * 1024)
case class BasicToken(value: String)
object BasicToken {
  private def generateBasicToken(user: String, pass: String): String =
    java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(s"$user:$pass".getBytes)

  def apply(user: String, pass: String): BasicToken = BasicToken(generateBasicToken(user, pass))
  def apply(up: (String, String)): BasicToken       = BasicToken(generateBasicToken(up._1, up._2))
}

case class UriAndOpt(
  uri: Uri,
  basicToken: Option[BasicToken] = None,
  bearerToken: Option[String] = None,
  headerFields: Option[List[Header.Raw]] = None,
  oks: List[Status] = statusOK
) {
  def addPath(s: String): UriAndOpt                           = copy(uri.addPath(s.stripPrefix("/")))
  def withQueryParams(params: Map[String, String]): UriAndOpt = copy(uri.withQueryParams(params))
}

object UriAndOpt {
  def apply(uri: Uri): UriAndOpt = UriAndOpt(uri = uri, basicToken = None, headerFields = None)
}
case class PropertiesSmartClient4s(
  httpClientTimeout: FiniteDuration,
  defaultCacheTTL: Option[FiniteDuration],
  maximumCacheSize: Option[Long],
  logData: Option[LogData],
  cacheDisabled: Boolean,
  httpClientHandShakeTimeout: FiniteDuration = 5.seconds
)
enum LogDirection(val direction: String) {
  case in    extends LogDirection("in")
  case out   extends LogDirection("out")
  case inout extends LogDirection("inout")
  case info  extends LogDirection("info")
}

case class Line(file: String, `class`: String, method: String, line: Int)

case class FEcontext(
  logEnable: Boolean,
  session: String,
  idParent: Option[UUID],
  user: String,
  forceSendParent: Boolean
)

case class HttpCall(
  session: Option[String],
  id: UUID,
  idParent: Option[UUID],
  direction: LogDirection,
  modifyDate: LocalDateTime,
  error: Boolean,
  requestPayload: Option[String],
  url: Option[String],
  method: String,
  responseStatus: Option[Status],
  responsePayload: Option[String],
  domain: String,
  user: Option[String],
  api: Option[String],
  line: Option[Line],
  idx: Option[List[Option[String]]],
  info: Option[String],
  fromCache: Option[Boolean]
)
