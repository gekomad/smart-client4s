package com.github.gekomad.smartClient4s

import cats.effect.IO
import com.github.gekomad.smartClient4s.cache.caffeine.CatsCaffeine
import com.github.gekomad.smartClient4s.model.PropertiesSmartClient4s
import com.github.gekomad.smartClient4s.util.TestUtil.PropertiesSmartClient4sIT
import com.github.gekomad.smartClient4s.model.FEcontext
import munit.CatsEffectSuite
import scala.concurrent.duration.DurationInt

class CaffeineTest extends CatsEffectSuite {
  private given Option[FEcontext]       = None
  private given PropertiesSmartClient4s = PropertiesSmartClient4sIT

  test("CatsCaffeine invalidate") {

    val cache = CatsCaffeine[Int, String](maximumSize = None, defaultTTL = 1.hour)
    for {
      _ <- cache.upSert(1, "foo")
      a <- cache.get(1)
      _ = assert(a.contains("foo"))
      _ <- cache.invalidate
      a <- cache.get(1)
      _ = assert(a.isEmpty)
    } yield ()
  }
  test("CatsCaffeine delete") {
    val cache = CatsCaffeine[Int, String](maximumSize = None, defaultTTL = 1.hour)
    for {
      _ <- cache.upSert(1, "foo")
      a <- cache.get(1)
      _ = assert(a.contains("foo"))
      _ <- cache.delete(1)
      a <- cache.get(1)
      _ = assert(a.isEmpty)
    } yield ()
  }

  test("CatsCaffeine test1") {
    val cache = CatsCaffeine[Int, String](maximumSize = None, defaultTTL = 400.millis)

    for {
      _ <- cache.upSert(1, "baz")
      _ <- cache.upSert(1, "foo", 5.seconds)
      a <- cache.get(1)
      _ = assert(a.contains("foo"))
      _ <- IO.sleep(1.seconds)
      b <- cache.get(1)
      _ = assert(b.contains("foo"))
    } yield ()
  }

  test("CatsCaffeine test") {
    val cache = CatsCaffeine[Int, String](maximumSize = None, defaultTTL = 400.millis)

    for {
      _ <- cache.upSert(1, "baz")
      _ <- cache.upSert(1, "foo")
      a <- cache.get(1)
      _ = assert(a.contains("foo"))
      _ <- IO.sleep(1.seconds)
      b <- cache.get(1)
      _ = assert(b.isEmpty)
    } yield ()
  }

  test("CatsCaffeine test Map") {
    val cache = CatsCaffeine[Int, String]()
    for {
      _ <- cache.upSert(Map(1 -> "baz", 2 -> "foo"))
      a <- cache.get(1)
      _ = assert(a.contains("baz"))
      a <- cache.get(2)
      _ = assert(a.contains("foo"))
      _    <- cache.delete(List(1, 2)) // Iterable type
      size <- cache.estimatedSize
      _ = assert(size == 0)
    } yield ()
  }

  test("CatsCaffeine test GC") {
    val cache = CatsCaffeine[String, List[Int]](maximumSize = None, defaultTTL = 800.millis)

    for {
      _  <- cache.upSert("a", List(1))
      _  <- cache.append("a", List(2, 3))
      s1 <- cache.estimatedSize
      _ = assert(s1 == 1)
      a <- cache.get("a")
      _ = assert(a.contains(List(1, 2, 3)))
      _  <- IO.sleep(1.seconds)
      _  <- cache.clean
      s2 <- cache.estimatedSize
      _ = assert(s2 == 0)
    } yield ()

  }

  test("CatsCaffeine test max size") {
    val cache = CatsCaffeine[Int, String](maximumSize = Some(1))

    for {
      _  <- cache.upSert(1, "foo")
      s1 <- cache.estimatedSize
      _ = assert(s1 == 1)
      _  <- cache.upSert(2, "bar")
      _  <- IO.sleep(1.seconds)
      s2 <- cache.estimatedSize
      _ = assert(s2 == 1)
    } yield ()

  }

}
