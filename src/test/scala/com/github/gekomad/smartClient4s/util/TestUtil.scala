package com.github.gekomad.smartClient4s.util

import com.github.gekomad.smartClient4s.model.PropertiesSmartClient4s
import scala.concurrent.duration.DurationInt

object TestUtil {

  case class MyData(name: String, age: Int)

  val PropertiesSmartClient4sIT = PropertiesSmartClient4s(
    httpClientTimeout = 20.seconds,
    defaultCacheTTL = Option(2.hour),
    maximumCacheSize = Some(3000),
    logData = None,
    cacheDisabled = false
  )

}
