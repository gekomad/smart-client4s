package com.github.gekomad.smartClient4s

import cats.effect.*
import cats.implicits.*
import com.github.gekomad.smartClient4s.HttpHeader.{CIcontentType, applicationJsonCT, textPlainCT}
import com.github.gekomad.smartClient4s.cache.caffeine.{CatsCaffeine, CatsCaffeineTrait}
import com.github.gekomad.smartClient4s.model.*
import com.github.gekomad.smartClient4s.util.*
import com.github.gekomad.smartClient4s.util.LogHelper.sourceInfo
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.{Encoder, Json}
import org.http4s.*
import org.http4s.Method.*
import org.http4s.Status.{InternalServerError, Ok}
import org.http4s.client.Client
import org.http4s.client.dsl.io.*
import org.http4s.headers.`Content-Type`
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.multipart.{Boundary, Multipart, Part}
import org.typelevel.ci.CIString
import org.typelevel.log4cats.*
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.net.http.HttpClient
import java.net.{InetSocketAddress, ProxySelector}
import java.time.{LocalDateTime, ZoneOffset}
import java.util.UUID
import scala.concurrent.duration.*
import com.github.gekomad.smartClient4s.util.DecoderCirce.given

object HttpHeader {
  val CIcontentType                                      = CIString("Content-Type")
  val acceptApplicationJson: Header.Raw                  = Header.Raw.apply(CIString("Accept"), "application/json")
  val applicationNdJson: Header.Raw                      = Header.Raw.apply(CIcontentType, "application/x-ndjson")
  val applicationJson: Header.Raw                        = Header.Raw.apply(CIcontentType, "application/json")
  val textPlain: Header.Raw                              = Header.Raw.apply(CIcontentType, "text/plain")
  val textXml: Header.Raw                                = Header.Raw.apply(CIcontentType, "text/xml")
  def nameValue(name: String, value: String): Header.Raw = Header.Raw.apply(CIString(name), value)
  def basicAuth(b: String): Header.Raw                   = Header.Raw.apply(CIString("Authorization"), s"Basic $b")
  def bearerAuth(b: String): Header.Raw                  = Header.Raw(CIString("Authorization"), s"Bearer $b")
  val applicationNdJsonCT: `Content-Type`                = `Content-Type`(MediaType.unsafeParse("application/x-ndjson"))
  val applicationJsonCT: `Content-Type`                  = `Content-Type`(MediaType.application.json)
  val textPlainCT: `Content-Type`                        = `Content-Type`(MediaType.text.plain)
  val textXmlCT: `Content-Type`                          = `Content-Type`(MediaType.text.xml)
}

object HttpClientProvider {
  private given LoggerFactory[IO]                   = Slf4jFactory.create[IO]
  private val logger: SelfAwareStructuredLogger[IO] = LoggerFactory[IO].getLogger

  def httpClientsResource(
    name: String,
    proxy: Option[ProxyUriPort]
  )(implicit propertiesSmartClient4s: PropertiesSmartClient4s): IO[Http4sClient] = {
    val c = httpClients(None).flatMap { httpClientLog =>
      httpClients(proxy).map { httpClient =>
        val logClient = Http4sClient(s"$name-logClient", httpClientLog, None)
        Http4sClient(name, httpClient, Some(logClient))
      }
    }
    logger.debug("httpClientsResource") *> c
  }
  private def httpClients(
    useProxy: Option[ProxyUriPort]
  )(implicit propertiesSmartClient4s: PropertiesSmartClient4s): IO[Client[IO]] = {
    import scala.jdk.DurationConverters.*
    val baseClient = HttpClient
      .newBuilder()
      .version(HttpClient.Version.HTTP_2)
      .connectTimeout(propertiesSmartClient4s.httpClientHandShakeTimeout.toJava)
    useProxy match {
      case Some(ProxyUriPort(host, port)) =>
        IO(baseClient.proxy(ProxySelector.of(new InetSocketAddress(host.toString, port.value))).build()).map(JdkHttpClient(_))
      case None => IO(baseClient.build()).map(JdkHttpClient(_))
    }
  }
}
case class PayloadError(uri: String, error: String = "Payload is not a Json", description: String)
case class Http4sClient(
  name: String,
  client: Client[IO],
  private val logClient: Option[Http4sClient]
)(implicit propertiesSmartClient4s: PropertiesSmartClient4s) {

  val starting: LocalDateTime            = LocalDateTime.now()
  private implicit val logger: PloggerIO = PloggerIO(this.getClass)

  val cache: Option[CatsCaffeineTrait[String, (String, Status)]] =
    if (propertiesSmartClient4s.cacheDisabled) None
    else Some(CatsCaffeine[String, (String, Status)](name, propertiesSmartClient4s.defaultCacheTTL, propertiesSmartClient4s.maximumCacheSize))
  def statistics: IO[(String, Long)]    = cache.map(_.estimatedSize).sequence.map(a => (name, a.getOrElse(0)))
  def estimatedSize: IO[Option[Long]]   = cache.map(_.estimatedSize).sequence
  def invalidateCache: IO[Option[Unit]] = cache.map(_.invalidate).sequence
  def cacheCleanUp: IO[Option[Unit]]    = cache.map(_.clean).sequence

  private def sendLogIfEnabled(
    f: FEcontext => IO[Option[Status]]
  )(implicit feContext: Option[FEcontext]): IO[Option[Status]] =
    feContext match {
      case Some(feCtx) if feCtx.logEnable && propertiesSmartClient4s.logData.isDefined => f(feCtx)
      case _                                                                           => IO(None)
    }

  def sendParentLog(
    uri: UriAndOpt,
    method: org.http4s.Method,
    body: Option[String],
//    cookies: Option[RequestCookie],
//    multipart: Option[String],
    api: Option[String],
    idx: Option[List[String]],
    res: (String, Status, Boolean),
    info: Option[String] = None
  )(implicit feContext: Option[FEcontext]): IO[Option[Status]] = logger.debug("sendParentLog") *> sendLogIfEnabled { feCtx =>
    sendToLog(
      idLog = feCtx.idParent.getOrElse(UUID.randomUUID),
      uri = Some(uri),
      method = method,
      body = body,
      direction = LogDirection.in,
//      cookies = cookies,
//      multipart = multipart,
      res = Some(res),
      feContext = feCtx.copy(idParent = None),
      api = api,
      idx = idx,
      info = info
    )
  }
  def sendInfoLog(info: String)(implicit feContext: Option[FEcontext]): IO[Option[Status]] = logger.debug("sendInfoLog") *> sendLogIfEnabled {
    feCtx =>
      sendToLog(
        idLog = UUID.randomUUID,
        uri = None,
        method = GET,
        body = None,
        direction = LogDirection.out,
//        cookies = None,
//        multipart = None,
        res = None,
        feContext = feCtx,
        info = Some(info),
        api = None,
        idx = None
      )
  }

  def sendChildLog(
    uri: UriAndOpt,
    method: org.http4s.Method,
    body: Option[String],
//    cookies: Option[RequestCookie],
//    multipart: Option[String],
    res: (String, Status, Boolean),
    info: Option[String] = None
  )(implicit feContext: Option[FEcontext]): IO[Option[Status]] = sendLogIfEnabled { feCtx =>
    sendToLog(
      idLog = UUID.randomUUID,
      Some(uri),
      method,
      body,
      direction = if (res._1.isBlank) LogDirection.out else LogDirection.inout,
//      cookies,
//      multipart,
      Some(res),
      feCtx,
      api = None,
      idx = None,
      info = info
    )
  }

  private def sendToLog(
    idLog: UUID,
    uri: Option[UriAndOpt],
    method: org.http4s.Method,
    body: Option[String],
    direction: LogDirection,
//    cookies: Option[RequestCookie],
//    multipart: Option[String],
    res: Option[(String, Status, Boolean)],
    feContext: FEcontext,
    api: Option[String],
    idx: Option[List[String]],
    info: Option[String]
  ): IO[Option[Status]] = propertiesSmartClient4s.logData match {
    case None => IO(None)
    case Some(LogData(kafkaLogUri, domain, _, maxPayloadSize)) =>
      given Retry             = Retry(2, 1.second)
      given Option[FEcontext] = Some(feContext)

      def truncatePayload(p: String): String = if (p.length > maxPayloadSize) s"${p.take(maxPayloadSize)}...truncated" else p
      val httpCall: HttpCall = HttpCall(
        session = Some(feContext.session),
        id = idLog,
        idParent = feContext.idParent,
        direction = direction,
        modifyDate = LocalDateTime.now(ZoneOffset.UTC),
        error = uri.fold(false)(uri1 => !uri1.oks.contains(res.fold(Ok)(_._2))),
        requestPayload = body.map(a => truncatePayload(a)),
        url = uri.map(_.uri.toString),
        method = method.toString,
        responseStatus = res.map(_._2),
        responsePayload = res.map(a => truncatePayload(a._1)),
        domain = domain,
        user = Some(feContext.user),
        api = api,
        line = Some(sourceInfo),
        idx = idx.map(_.map(a => Some(a))),
        info = info,
        fromCache = res.fold(Some(false))(a => Some(a._3))
      )

      logClient.map { x =>
        for {
          aa <- x.callNoCache(
            kafkaLogUri,
            POST,
            Some(httpCall.asJson.spaces2),
            None,
            None,
            timeout = propertiesSmartClient4s.httpClientHandShakeTimeout,
            applicationJsonCT
          )
          _ <- logger.debug(s"sendToLog $aa")
        } yield aa._2
      }.sequence

  }
  private def sha256(s: String): String = java.security.MessageDigest.getInstance("SHA-256").digest(s.getBytes).map("%02x".format(_)).mkString
  private def shaCode(
    uri: UriAndOpt,
    method: org.http4s.Method,
    body: Option[String],
    cookies: Option[RequestCookie],
    multipart: Option[String]
  ): String = {
    def shaRequest: String = {
      val url        = uri.toString
      val method1    = method.toString
      val body1      = body.toString
      val cookies1   = cookies.toString
      val multipart1 = multipart.toString
      val source     = s"$url-$method1-$body1-$cookies1-$multipart1"
      val a          = sha256(source)
      a
    }
    shaRequest
  }
  def invalidate(
    uri: UriAndOpt,
    method: org.http4s.Method,
    body: Option[String] = None,
    cookies: Option[RequestCookie] = None,
    multipart: Option[String] = None
  )(implicit feContext: Option[FEcontext]): IO[Unit] = cache match {
    case Some(cache1) =>
      val sha = shaCode(uri, method, body, cookies, multipart)
      logger.debug(s"invalidate cache $sha") *> cache1.delete(sha)(using None)
    case _ => IO(())
  }

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
    private def toJson(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit
      feContext: Option[FEcontext]
    ): IO[(Json, Status, Boolean)] = {
      if (payload.isBlank) IO((Json.Null, status, fromCache))
      else
        io.circe.parser.parse(payload) match {
          case Left(_) =>
            val st = if (status.isSuccess) InternalServerError else status
            logger.error(s"decode Json error: $payload") *>
              IO((PayloadError(uri = uri.uri.toString, description = payload).asJson, st, fromCache))
          case Right(v) => IO((v, status, fromCache))
        }
    }
    // Json -> Json
    given CallCodec[Json, Json] with {
      def encodeBody(in: Option[Json]): Option[String] = in.map(_.spaces2)
      def contentType: `Content-Type`                  = applicationJsonCT
      def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit
        feContext: Option[FEcontext]
      ): IO[(Json, Status, Boolean)] = toJson(payload, status, fromCache, uri)
    }

    // Json -> Unit
    given CallCodec[Json, Unit] with {
      def encodeBody(in: Option[Json]): Option[String] = in.map(_.spaces2)
      def contentType: `Content-Type`                  = applicationJsonCT
      def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit feContext: Option[FEcontext]) =
        IO(((), status, fromCache))
    }
    // Json -> String
    given CallCodec[Json, String] with {
      def encodeBody(in: Option[Json]): Option[String] = in.map(_.spaces2)
      def contentType: `Content-Type`                  = applicationJsonCT
      def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit feContext: Option[FEcontext]) =
        IO((payload, status, fromCache))
    }
    // String -> String
    given CallCodec[String, String] with {
      def encodeBody(in: Option[String]): Option[String] = in
      def contentType: `Content-Type`                    = textPlainCT
      def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit feContext: Option[FEcontext]) =
        IO((payload, status, fromCache))
    }

    // String -> Json
    given CallCodec[String, Json] with {
      def encodeBody(in: Option[String]): Option[String] = in
      def contentType: `Content-Type`                    = textPlainCT
      def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit
        feContext: Option[FEcontext]
      ): IO[(Json, Status, Boolean)] = toJson(payload, status, fromCache, uri)
    }

    // String -> Unit
    given CallCodec[String, Unit] with {
      def encodeBody(in: Option[String]): Option[String] = in
      def contentType: `Content-Type`                    = textPlainCT
      def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit
        feContext: Option[FEcontext]
      ): IO[(Unit, Status, Boolean)] = IO(((), status, fromCache))
    }
    // Unit -> Json
    given CallCodec[Unit, Json] with {
      def encodeBody(in: Option[Unit]): Option[String] = None
      def contentType: `Content-Type`                  = textPlainCT
      def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit
        feContext: Option[FEcontext]
      ): IO[(Json, Status, Boolean)] =
        toJson(payload, status, fromCache, uri)
    }

    // Unit -> Unit
    given CallCodec[Unit, Unit] with {
      def encodeBody(in: Option[Unit]): Option[String] = None
      def contentType: `Content-Type`                  = textPlainCT
      def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit feContext: Option[FEcontext]) =
        IO(((), status, fromCache))
    }
    // Unit -> String
    given CallCodec[Unit, String] with {
      def encodeBody(in: Option[Unit]): Option[String] = None
      def contentType: `Content-Type`                  = textPlainCT
      def decodeResponse(payload: String, status: Status, fromCache: Boolean, uri: UriAndOpt)(implicit feContext: Option[FEcontext]) =
        IO((payload, status, fromCache))
    }
  }

  def call[In, Out](
    uri: UriAndOpt,
    method: org.http4s.Method,
    body: Option[In] = None,
    cookies: Option[RequestCookie] = None,
    multipart: Option[String] = None,
    ttlCache: Option[FiniteDuration] = None,
    timeout: FiniteDuration = propertiesSmartClient4s.httpClientTimeout,
    noLog: Boolean = false
  )(implicit
    retry: Retry,
    feContext: Option[FEcontext] = None,
    codec: CallCodec[In, Out]
  ): IO[(Out, Status, Boolean)] = {
    val body1 = codec.encodeBody(body)
    val ct    = codec.contentType
    val res = cache match {
      case Some(cache1) if ttlCache.isDefined =>
        for {
          _ <- IO(())
          hash = shaCode(uri, method, body1, cookies, multipart)
          fromCache <- cache1.get(hash)
          res <- fromCache match {
            case Some(value) => logger.debug(s"probe cache: FOUND uri: ${uri.uri}  hash: $hash") *> IO((value._1, value._2, true))
            case None =>
              for {
                _ <- logger.debug(s"probe cache: NOT FOUND uri: ${uri.uri} hash: $hash")
                (payload, status) <- callNoCache(
                  uri,
                  method,
                  body1,
                  cookies,
                  multipart,
                  timeout,
                  ct
                )
                _ <- ttlCache match {
                  case Some(ttl) if uri.oks.contains(status) => cache1.upSert(hash, (payload, status), ttl)
                  case _                                     => IO(())
                }
              } yield (payload, status, false)
          }
        } yield res
      case _ =>
        for {
          res <- callNoCache(
            uri,
            method,
            body1,
            cookies,
            multipart,
            timeout,
            ct
          ).map(a => (a._1, a._2, false))
        } yield res
    }

    res.flatMap { res1 =>
      (if (noLog) IO.unit else sendChildLog(uri, method, body1, res1)) *>
        codec.decodeResponse(res1._1, res1._2, res1._3, uri)
    }
  }

  private def callNoCache(
    uri: UriAndOpt,
    method: org.http4s.Method,
    body: Option[String],
    cookies: Option[RequestCookie],
    multipart: Option[String],
    timeout: FiniteDuration,
    ct: `Content-Type`
  )(implicit retry: Retry, feContext: Option[FEcontext]): IO[(String, Status)] = {
    val headers = uri.headerFields.map(b => Headers(b.map(a => Header.Raw(a.name, a.value))))
    def addBodyMultipart(name: String)(req: Request[IO]): Request[IO] =
      body match {
        case Some(value) =>
          val mlt =
            Multipart[IO](Vector(Part.formData(name, value)), Boundary("----FormBoundaryEmABDsBKjG7QEqu"))
          req.withEntity(mlt).withHeaders(mlt.headers)
        case None => req
      }

    def addBody(req: Request[IO]): Request[IO] = body.fold(req) { b =>
      req.withEntity(b)
    }

    def addHeaders(req: Request[IO]): Request[IO] =
      headers.fold(req)(f =
        b =>
          req.withHeaders(Headers {
            if (b.headers.exists(_.name == CIcontentType))
              req.headers.headers.filterNot(_.name == CIcontentType) ::: b.headers
            else req.headers.headers ::: b.headers
          })
      )

    def addCookie(req: Request[IO]): Request[IO] = cookies.fold(req)(b => req.addCookie(b))

    def addBasicAuth(req: Request[IO]): Request[IO] =
      uri.basicToken.fold(req)(b => req.withHeaders(req.headers.put(HttpHeader.basicAuth(b.value))))

    def addBearerAuth(req: Request[IO]): Request[IO] =
      uri.bearerToken.fold(req)(b => req.withHeaders(req.headers.put(HttpHeader.bearerAuth(b))))

    def addPayloadHeader(req: Request[IO]): Request[IO] =
      if (headers.exists(_.headers.exists(_.name == CIcontentType))) req
      else req.withHeaders(req.headers.put(ct))

    def decorate(req: Request[IO]): Request[IO] =
      multipart match {
        case Some(name) => ((addBodyMultipart(name)(_)) andThen addCookie andThen addBasicAuth andThen addBearerAuth)(req)
        case None =>
          (addBody andThen addHeaders andThen addCookie andThen addBasicAuth andThen addBearerAuth andThen addPayloadHeader)(req)
      }

    val doRequest: Request[IO] = method match {
      case POST    => decorate(POST(uri.uri))
      case PUT     => decorate(PUT(uri.uri))
      case PATCH   => decorate(PATCH(uri.uri))
      case DELETE  => decorate(DELETE(uri.uri))
      case OPTIONS => decorate(OPTIONS(uri.uri))
      case GET     => decorate(GET(uri.uri))
      case HEAD    => decorate(HEAD(uri.uri))
      case _       => throw new NotImplementedError(s"no method found: $method")
    }

    retry
      .retry {
        for {
          _   <- logger.debug(s"call with $retry ($method) $uri headers: (${doRequest.headers.headers.mkString(",")})...")
          res <- client.run(doRequest).use(r => r.as[String].map(i => (i, r.status))).timeout(timeout)
          _   <- logger.debug(s"call ($method) $uri ok")
        } yield res
      }
      .handleErrorWith { e =>
        val x = s"${uri.uri}: ${e.toString}"
        logger.error(s"$name call ($method) ${uri.uri} body: $body ko ${e.toString}") *> IO((x, InternalServerError))
      }
  }
}
