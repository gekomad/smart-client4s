package com.github.gekomad.smartClient4s

import cats.effect.*
import com.github.gekomad.smartClient4s.CacheType.CaffeineType
import com.github.gekomad.smartClient4s.cache.CacheTrait
import com.github.gekomad.smartClient4s.model.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.jdkhttpclient.JdkHttpClient
import org.typelevel.log4cats.*
import org.typelevel.log4cats.slf4j.Slf4jFactory

import java.net.http.HttpClient
import java.net.{InetSocketAddress, ProxySelector}

private final case class CacheBuilderTrait[K, V](
  cache: CacheTrait[K, V]
)
private trait CacheBuilder {
  def create[K, V](name: Option[String] = None)(implicit propertiesSmartClient4s: PropertiesSmartClient4s): Option[CacheTrait[K, V]]
}

enum CacheType {
  case CaffeineType, RedisType
}

object CaffeineBuilder extends CacheBuilder {
  import com.github.gekomad.smartClient4s.cache.caffeine.CatsCaffeine
  override def create[K, V](name: Option[String] = None)(implicit propertiesSmartClient4s: PropertiesSmartClient4s): Option[CacheTrait[K, V]] =
    propertiesSmartClient4s.cacheConf.flatMap(cache => CatsCaffeine[K, V](name, cache.defaultTTL, cache.maxSize))
}
object RedisBuilder extends CacheBuilder {
  override def create[K, V](name: Option[String] = None)(implicit propertiesSmartClient4s: PropertiesSmartClient4s): Option[CacheTrait[K, V]] =
    throw new RuntimeException("Not implemented")
}

private def getCache[K, V](cacheType: CacheType, name: Option[String] = None)(implicit
  propertiesSmartClient4s: PropertiesSmartClient4s
): Option[CacheBuilderTrait[K, V]] = {
  val cache = cacheType match {
    case CacheType.CaffeineType => CaffeineBuilder.create[K, V](name)
    case CacheType.RedisType    => RedisBuilder.create[K, V](name)
  }
  cache.map(CacheBuilderTrait[K, V])
}

object HttpClientProvider {
  private given LoggerFactory[IO]                   = Slf4jFactory.create[IO]
  private val logger: SelfAwareStructuredLogger[IO] = LoggerFactory[IO].getLogger
  type HashKey = Option[String]

  def httpClientsResource(proxy: Option[ProxyUriPort], name: Option[String] = None, cacheType: CacheType = CaffeineType)(implicit
    propertiesSmartClient4s: PropertiesSmartClient4s
  ): IO[Http4sClient] = {
    val c = httpClients(None).flatMap { httpClientLog =>
      httpClients(proxy).map { httpClient =>
        val logClient = Http4sClient(
          name = name.map(a => s"$a-logClient"),
          client = httpClientLog,
          logClient = None,
          cache = None
        )
        Http4sClient(
          name,
          httpClient,
          Some(logClient),
          cache = getCache[String, (String, Status)](cacheType).map(_.cache)
        )
      }
    }
    logger.debug("httpClientsResource") *> c
  }

  private def httpClients(useProxy: Option[ProxyUriPort])(implicit propertiesSmartClient4s: PropertiesSmartClient4s): IO[Client[IO]] = {
    import scala.jdk.DurationConverters.*
    val baseClient = HttpClient
      .newBuilder()
      .version(HttpClient.Version.HTTP_2)
      .connectTimeout(propertiesSmartClient4s.httpClientConf.handShakeTimeout.toJava)

    val cl = useProxy match {
      case Some(ProxyUriPort(host, port, proxyUser, proxyPass)) =>
        val c = baseClient.proxy(ProxySelector.of(new InetSocketAddress(host.toString, port.value)))
        proxyUser match {
          case Some(user) =>
            val pass = proxyPass.filter(_.nonEmpty).fold(Array.emptyCharArray)(_.toCharArray)
            c.authenticator(new java.net.Authenticator {
              override def getPasswordAuthentication: java.net.PasswordAuthentication =
                new java.net.PasswordAuthentication(user, pass)
            })
          case None => c
        }
      case None => baseClient
    }
    IO(JdkHttpClient[IO](cl.build()))
  }
}
