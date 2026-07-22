SmartClient4s
=========
Http4s client with cache and log
---
    Need JDK 11+

```scala
import io.circe.Json
import org.http4s.Method.GET
import org.http4s.Uri
import com.github.gekomad.smartClient4s.model.{BasicToken, CacheConf, HttpClientConf, PropertiesSmartClient4s, UriAndOpt}
import scala.concurrent.duration.*
import com.github.gekomad.smartClient4s.util.Retry

given Retry = Retry(1, 1.second)
given PropertiesSmartClient4s = PropertiesSmartClient4s(
  httpClientConf = HttpClientConf(timeout = 20.seconds),
  logConf = None,
  cacheConf = Some(CacheConf(defaultTTL = 2.hour, maxSize = Some(3000)))
)
val body: Json = ???
val uri: Uri   = ???
val basicToken = BasicToken.apply("user", "pass")
for {
  client <- HttpClientProvider.httpClientsResource(proxy = None)
  (payload, status, fromCache) <- client.call[Json /* input type */, String /* output output*/ ](
    UriAndOpt(uri = uri, basicToken = Some(basicToken)),
    body = Some(body),
    method = GET,
    timeout = 10.second,
    ttlCache = Some(1.hour)
  )
} yield {
  assert(status.isSuccess)
  assert(payload == "Hello")
  assert(fromCache == true)
}
```