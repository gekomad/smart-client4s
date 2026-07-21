package com.github.gekomad.smartClient4s.util

import com.github.gekomad.smartClient4s.model.{CacheConf, HttpClientConf, PropertiesSmartClient4s}

import scala.concurrent.duration.DurationInt

object TestUtil {

  case class MyData(name: String, age: Int)

  val PropertiesSmartClient4sIT = PropertiesSmartClient4s(
    httpClientConf = HttpClientConf(timeout = 20.seconds),
    logConf = None,
    cacheConf = Some(CacheConf(defaultTTL = 2.hour, maxSize = Some(3000)))
  )

}
