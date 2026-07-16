package com.github.gekomad.smartClient4s.util

import cats.*
import scala.concurrent.duration.*
import cats.effect.IO
import retry.*
import retry.ResultHandler.retryOnAllErrors
import retry.RetryDetails.NextStep.*
import retry.RetryPolicies.*
import com.github.gekomad.smartClient4s.model.FEcontext

case class Retry(maxRetries: Int, delay: FiniteDuration) {

  private val logger = PloggerIO(this.getClass)
  private def logError(err: Throwable, details: RetryDetails)(implicit feContext: Option[FEcontext]): IO[Unit] =
    details.nextStepIfUnsuccessful match {
      case DelayAndRetry(nextDelay: FiniteDuration) =>
        logger.warn(s"Failed. So far we have retried ${details.retriesSoFar} $err times. $nextDelay")
      case GiveUp => logger.warn(s"Giving up after ${details.retriesSoFar} retries $err")
    }

  private val retryPolicy = limitRetries[IO](maxRetries = maxRetries) join constantDelay[IO](delay)
  def retry[A](action: => IO[A])(implicit feContext: Option[FEcontext]): IO[A] =
    retryingOnErrors(action)(policy = retryPolicy, errorHandler = retryOnAllErrors(logError))

}
