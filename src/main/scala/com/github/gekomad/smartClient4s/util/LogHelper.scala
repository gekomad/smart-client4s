package com.github.gekomad.smartClient4s.util

import org.http4s.Request
import org.slf4j.{Logger, LoggerFactory}
import org.typelevel.ci
import org.typelevel.ci.CIStringSyntax

import java.util.UUID
import scala.util.Try
import cats.effect.IO
import com.github.gekomad.smartClient4s.model.{FEcontext, Line}

def extractFEcontextFromHeader(req: Request[IO]): FEcontext = {
  val user: String    = req.headers.get(ci"X-vega-user").map(_.head.value).getOrElse("unknown") // TODO rename vega
  val session: String = req.headers.get(ci"X-vega-session").map(_.head.value).getOrElse(f"#${scala.util.Random.nextInt(0xffffff + 1)}%06x")
  val useLog: Boolean = req.headers.get(ci"X-vega-useLog").map(_.head.value).getOrElse("true").toBoolean
  val (idParent, forceSendParent) = {
    req.headers.get(ci"X-vega-idParent").map(_.head.value).flatMap(a => Try(UUID.fromString(a)).toOption) match {
      case Some(value) => (value, false)
      case None        => (UUID.randomUUID, true)
    }
  }

  FEcontext(
    logEnable = useLog,
    session = session,
    idParent = Some(idParent),
    user = user,
    forceSendParent = forceSendParent
  )
}

object LogHelper {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  @inline def sourceInfo: Line = {
    val stackTrace = Thread.currentThread().getStackTrace
    val caller     = stackTrace(4)
    val fileName   = caller.getFileName
    val className  = caller.getClassName
    val lineNumber = caller.getLineNumber
    val methodName = caller.getMethodName
    Line(
      file = Option(fileName).getOrElse("<unknown file name>"),
      `class` = className,
      method = methodName,
      line = lineNumber
    )
  }
}
