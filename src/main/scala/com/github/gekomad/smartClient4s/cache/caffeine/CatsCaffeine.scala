package com.github.gekomad.smartClient4s.cache.caffeine

import com.github.benmanes.caffeine.cache.{Cache, Caffeine, Expiry}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import cats.effect.IO
import com.github.gekomad.smartClient4s.model.PropertiesSmartClient4s
import com.github.gekomad.smartClient4s.util.PloggerIO
import com.github.gekomad.smartClient4s.model.FEcontext

case class ElementWithTTL[V](value: V, ttl: FiniteDuration)

trait CatsCaffeineTrait[@specialized(Int, Long) K, @specialized(Int, Long) V] {
  val cacheName: String
  def append[A](key: K, values: List[A], ttl: Option[FiniteDuration] = None)(using ev: V <:< List[A])(implicit
    feContext: Option[FEcontext]
  ): IO[Unit]
  def get(key: K)(implicit feContext: Option[FEcontext]): IO[Option[V]]
  def upSert(key: K, value: V)(implicit feContext: Option[FEcontext]): IO[Unit]
  def upSert(key: K, value: V, ttl: Option[FiniteDuration])(implicit feContext: Option[FEcontext]): IO[Unit]
  def upSert(key: K, value: V, ttl: FiniteDuration)(implicit feContext: Option[FEcontext]): IO[Unit]
  def upSert(map: Map[K, V], ttl: Option[FiniteDuration] = None)(implicit feContext: Option[FEcontext], env: PropertiesSmartClient4s): IO[Unit]
  def delete(key: K)(implicit feContext: Option[FEcontext]): IO[Unit]
  def delete(keys: Iterable[K])(implicit feContext: Option[FEcontext]): IO[Unit]
  def invalidate: IO[Unit]
  def clean: IO[Unit]
  def estimatedSize: IO[Long]
}

private class CatsCaffeineEnable[@specialized(Int, Long) K, @specialized(Int, Long) V](
  name: String,
  val defaultTTL: Option[FiniteDuration],
  private val cache: Cache[K, ElementWithTTL[V]]
) extends CatsCaffeineTrait[K, V] {
  override val cacheName: String         = name
  private implicit val logger: PloggerIO = PloggerIO(this.getClass)
  override def append[A](key: K, values: List[A], ttl: Option[FiniteDuration] = None)(using
    ev: V <:< List[A]
  )(implicit feContext: Option[FEcontext]): IO[Unit] =
    for {
      current <- get(key)
      _ <- current match {
        case Some(value) => upSert(key, (value ++ values).distinct.asInstanceOf[V], ttl)
        case None        => upSert(key, values.asInstanceOf[V], ttl)
      }
    } yield ()

  override def get(key: K)(implicit feContext: Option[FEcontext]): IO[Option[V]] = {
    val a =
      IO(Option(cache.getIfPresent(key))).flatTap { opt =>
        logger.debug(s"$name get $key found from cache: ${opt.isDefined}")
      }
    a.map { _.fold(None: Option[V])(a => Some(a.value)) }
  }

  def upSert(key: K, value: V)(implicit feContext: Option[FEcontext]): IO[Unit] = upSert(key, value, None)
  def upSert(key: K, value: V, ttl: Option[FiniteDuration])(implicit feContext: Option[FEcontext]): IO[Unit] =
    logger.debug(s"$name upSert $key") *> IO(
      cache.put(
        key,
        ElementWithTTL(
          value,
          ttl.getOrElse(
            defaultTTL.getOrElse(FiniteDuration(Long.MaxValue, NANOSECONDS))
          )
        )
      )
    )

  def upSert(key: K, value: V, ttl: FiniteDuration)(implicit feContext: Option[FEcontext]): IO[Unit] =
    logger.debug(s"$name upSert $key") *> IO(cache.put(key, ElementWithTTL(value, ttl)))

  def upSert(map: Map[K, V], ttl: Option[FiniteDuration] = None)(implicit feContext: Option[FEcontext], env: PropertiesSmartClient4s): IO[Unit] =
    if (map.isEmpty) IO(())
    else {
      val a = map.map { a =>
        a._1 -> ElementWithTTL(
          a._2,
          ttl.getOrElse(
            env.defaultCacheTTL.getOrElse(FiniteDuration(Long.MaxValue, NANOSECONDS))
          )
        )
      }
      logger.debug(s"$name upSert batch size=${map.size}") *> IO(cache.putAll(a.asJava))
    }
  def delete(key: K)(implicit feContext: Option[FEcontext]): IO[Unit] = logger.debug(s"$name delete $key") *> IO(cache.invalidate(key))
  def delete(keys: Iterable[K])(implicit feContext: Option[FEcontext]): IO[Unit] =
    if (keys.isEmpty) IO(()) else logger.debug(s"$name delete batch size=${keys.size}") *> IO(cache.invalidateAll(keys.asJava))
  def invalidate: IO[Unit]    = logger.debug(s"$name invalidate")(using None) *> IO(cache.invalidateAll())
  def clean: IO[Unit]         = logger.debug(s"$name clean")(using None) *> IO(cache.cleanUp())
  def estimatedSize: IO[Long] = IO(cache.estimatedSize())

}

private class CatsCaffeineDisabled[@specialized(Int, Long) K, @specialized(Int, Long) V](
  val defaultTTL: Option[FiniteDuration],
  private val cache: Cache[K, ElementWithTTL[V]]
) extends CatsCaffeineTrait[K, V] {
  override val cacheName: String         = ""
  private implicit val logger: PloggerIO = PloggerIO(this.getClass)
  println(cache)
  println(logger)
  def append[A](key: K, values: List[A], ttl: Option[FiniteDuration] = None)(using ev: V <:< List[A])(implicit
    feContext: Option[FEcontext]
  ): IO[Unit] = IO(())
  def get(key: K)(implicit feContext: Option[FEcontext]): IO[Option[V]]                                      = IO(None)
  def upSert(key: K, value: V)(implicit feContext: Option[FEcontext]): IO[Unit]                              = upSert(key, value, None)
  def upSert(key: K, value: V, ttl: Option[FiniteDuration])(implicit feContext: Option[FEcontext]): IO[Unit] = IO(())
  def upSert(key: K, value: V, ttl: FiniteDuration)(implicit feContext: Option[FEcontext]): IO[Unit]         = IO(())
  def upSert(map: Map[K, V], ttl: Option[FiniteDuration] = None)(implicit feContext: Option[FEcontext], env: PropertiesSmartClient4s): IO[Unit] =
    IO(())
  def delete(key: K)(implicit feContext: Option[FEcontext]): IO[Unit]            = IO(())
  def delete(keys: Iterable[K])(implicit feContext: Option[FEcontext]): IO[Unit] = IO(())
  def invalidate: IO[Unit]                                                       = IO(())
  def clean: IO[Unit]                                                            = IO(())
  def estimatedSize: IO[Long]                                                    = IO(0)
}

object CatsCaffeine {
  import scala.util.chaining.*
  def apply[K, V](using env: PropertiesSmartClient4s)(
    cacheName: String,
    defaultTTL: Option[FiniteDuration] = env.defaultCacheTTL,
    maximumSize: Option[Long] = env.maximumCacheSize
  ): CatsCaffeineTrait[K, V] = {

    val expiryPolicy = new Expiry[K, ElementWithTTL[V]] {

      override def expireAfterCreate(key: K, item: ElementWithTTL[V], currentTime: Long): Long =
        item.ttl.toNanos

      override def expireAfterUpdate(
        key: K,
        item: ElementWithTTL[V],
        currentTime: Long,
        currentDuration: Long
      ): Long = item.ttl.toNanos

      override def expireAfterRead(
        key: K,
        item: ElementWithTTL[V],
        currentTime: Long,
        currentDuration: Long
      ): Long = currentDuration
    }

    val ref: Cache[K, ElementWithTTL[V]] =
      Caffeine
        .newBuilder()
        .tap(b => maximumSize.foreach(b.maximumSize))
        .tap(_.expireAfter(expiryPolicy))
        .build[K, ElementWithTTL[V]]

    if (env.cacheDisabled) new CatsCaffeineDisabled[K, V](defaultTTL = None, cache = ref)
    else new CatsCaffeineEnable[K, V](name = cacheName, defaultTTL = defaultTTL, cache = ref)
  }
}
