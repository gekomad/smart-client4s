package com.github.gekomad.smartClient4s.util
import com.github.gekomad.smartClient4s.model.FEcontext

private def parentLogId(implicit feContext: Option[FEcontext]): String = feContext.flatMap(_.idParent).fold("")(id => s"parentLogId: $id")

case class Plogger(clazz: Class[?]) {
  import org.slf4j.{Logger, LoggerFactory}
  private val log: Logger = LoggerFactory.getLogger(clazz)

  inline def trace(inline msg: String)(implicit feContext: Option[FEcontext]): Unit = log.trace(s"$parentLogId $msg")
  inline def info(inline msg: String)(implicit feContext: Option[FEcontext]): Unit  = log.info(s"$parentLogId $msg")
  inline def warn(inline msg: String)(implicit feContext: Option[FEcontext]): Unit  = log.warn(s"$parentLogId $msg")
  inline def debug(inline msg: String)(implicit feContext: Option[FEcontext]): Unit = log.debug(s"$parentLogId $msg")
  inline def error(inline msg: String)(implicit feContext: Option[FEcontext]): Unit = log.error(s"$parentLogId $msg")

  inline def trace(inline msg: String, e: Throwable)(implicit feContext: Option[FEcontext]): Unit = log.trace(s"$parentLogId $msg", e)
  inline def info(inline msg: String, e: Throwable)(implicit feContext: Option[FEcontext]): Unit  = log.info(s"$parentLogId $msg", e)
  inline def warn(inline msg: String, e: Throwable)(implicit feContext: Option[FEcontext]): Unit  = log.warn(s"$parentLogId $msg", e)
  inline def debug(inline msg: String, e: Throwable)(implicit feContext: Option[FEcontext]): Unit = log.debug(s"$parentLogId $msg", e)
  inline def error(inline msg: String, e: Throwable)(implicit feContext: Option[FEcontext]): Unit = log.error(s"$parentLogId $msg", e)

}

case class PloggerIO(clazz: Class[?]) {
  import cats.effect.IO
  import org.slf4j.{Logger, LoggerFactory}
  private val log: Logger = LoggerFactory.getLogger(clazz)

  inline def trace(inline msg: String)(implicit feContext: Option[FEcontext]): IO[Unit] = IO(log.trace(s"$parentLogId $msg"))
  inline def info(inline msg: String)(implicit feContext: Option[FEcontext]): IO[Unit]  = IO(log.info(s"$parentLogId $msg"))
  inline def warn(inline msg: String)(implicit feContext: Option[FEcontext]): IO[Unit]  = IO(log.warn(s"$parentLogId $msg"))
  inline def debug(inline msg: String)(implicit feContext: Option[FEcontext]): IO[Unit] = IO(log.debug(s"$parentLogId $msg"))
  inline def error(inline msg: String)(implicit feContext: Option[FEcontext]): IO[Unit] = IO(log.error(s"$parentLogId $msg"))

  inline def trace(inline msg: String, e: Throwable)(implicit feContext: Option[FEcontext]): IO[Unit] = IO(log.trace(s"$parentLogId $msg", e))
  inline def info(inline msg: String, e: Throwable)(implicit feContext: Option[FEcontext]): IO[Unit]  = IO(log.info(s"$parentLogId $msg", e))
  inline def warn(inline msg: String, e: Throwable)(implicit feContext: Option[FEcontext]): IO[Unit]  = IO(log.warn(s"$parentLogId $msg", e))
  inline def debug(inline msg: String, e: Throwable)(implicit feContext: Option[FEcontext]): IO[Unit] = IO(log.debug(s"$parentLogId $msg", e))
  inline def error(inline msg: String, e: Throwable)(implicit feContext: Option[FEcontext]): IO[Unit] = IO(log.error(s"$parentLogId $msg", e))

}
