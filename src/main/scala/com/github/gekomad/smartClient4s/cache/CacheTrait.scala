package com.github.gekomad.smartClient4s.cache

import cats.effect.IO
import com.github.gekomad.smartClient4s.model.{FEcontext, PropertiesSmartClient4s}
import scala.concurrent.duration.*

case class ElementWithTTL[V](value: V, ttl: FiniteDuration)

trait CacheTrait[@specialized(Int, Long) K, @specialized(Int, Long) V] {
  val cacheName: Option[String]
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
