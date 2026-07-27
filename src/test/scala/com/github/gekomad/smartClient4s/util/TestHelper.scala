package com.github.gekomad.smartClient4s.util

import com.github.gekomad.smartClient4s.model.{BasicToken, UriAndOpt}
import io.circe.Json
import io.circe.parser.parse
import org.http4s.implicits.uri

object TestHelper {
  type JsonError = Json

  val fetchDataUrl: UriAndOpt =
    UriAndOpt(uri = uri"http://localhost:9111/api/get_long_service", basicToken = Some(BasicToken.apply("admin", "pass123")))

  val fetchDataPayload: String => Json = (cc: String) => parse(s"""{"Code": "$cc"}""").getOrElse(???)

}
