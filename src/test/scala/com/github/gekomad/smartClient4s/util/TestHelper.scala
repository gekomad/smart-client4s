package com.github.gekomad.smartClient4s.util

import cats.effect.IO
import com.github.gekomad.smartClient4s.Http4sClient
import com.github.gekomad.smartClient4s.model.{BasicToken, FEcontext, UriAndOpt}
import com.github.gekomad.smartClient4s.util.PloggerIO
import io.circe.Json
import io.circe.parser.parse
import org.http4s.Method.POST
import org.http4s.implicits.uri

object TestHelper {
  private val logger = PloggerIO(this.getClass)
  type JsonError = Json

  val fetchDataUrl: UriAndOpt =
    UriAndOpt(uri = uri"http://localhost:9111/api/get_long_service", basicToken = Some(BasicToken.apply("admin", "pass123")))

  val fetchDataPayload: String => Json = (cc: String) => parse(s"""{"Code": "$cc"}""").getOrElse(???)

  def invalidateCC(cc: String)(implicit client: Http4sClient, feContext: Option[FEcontext]): IO[Unit] =
    logger.debug(s"invalidate cc: $cc") *> client.invalidate(
      fetchDataUrl,
      POST,
      body = Some(fetchDataPayload(cc).spaces2),
      None
    )

}
