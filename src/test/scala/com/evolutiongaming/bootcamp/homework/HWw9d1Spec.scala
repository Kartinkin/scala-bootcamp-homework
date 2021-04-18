package com.evolutiongaming.bootcamp.homework

import EffectsHomework1.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

import java.util.concurrent.atomic.AtomicInteger

class EffectsHomework1Spec extends AnyFlatSpec with Matchers {
  "map" should "apply f to value" in {
    forAll { (x: Int) =>
      IO(x).map { i => i + 1 }.unsafeRunSync() shouldEqual x + 1
    }
  }

  "flatMap" should "apply f to value" in {
    forAll { (x: Int) =>
      IO(x).flatMap { i => IO(i+1) }.unsafeRunSync() shouldEqual x + 1
    }
  }

  "*>" should "run both" in {
    val count = new AtomicInteger(0)
    (IO(count.incrementAndGet()) *> IO(count.incrementAndGet())).unsafeRunSync()
    count.get() shouldEqual 2
  }

  "*>" should "return second" in {
    forAll { (x: Int, y: Int) =>
      (IO(x) *> IO(y)).unsafeRunSync() shouldEqual y
    }
  }

  "*>" should "not run second in case of error" in {
    val count = new AtomicInteger(0)

    an[Exception] should be thrownBy (
      (IO[Int]("aaaa".toInt) *> IO(count.incrementAndGet()))
      .unsafeRunSync()
    )
    count.get() shouldEqual 0
  }

  "as" should "replace value" in {
    forAll { (x: Int, y: Int) =>
      (IO(x) as y).unsafeRunSync() shouldEqual y
    }
  }

  "void" should "return Unit" in {
    forAll { (x: Int) =>
      IO(x).void.unsafeRunSync() shouldEqual ()
    }
  }

  "attempt" should "return Right" in {
    forAll { (x: Int) =>
      IO(x).attempt.unsafeRunSync() shouldEqual Right(x)
    }
  }

  "attempt" should "return Left" in {
    forAll { (e: Exception) =>
      IO[Int](throw e).attempt.unsafeRunSync() shouldEqual Left(e)
    }
  }

  "option" should "return Some" in {
    forAll { (x: Int) =>
      IO(x).option.unsafeRunSync() shouldEqual Some(x)
    }
  }

  "option" should "return None" in {
    IO(throw new Exception("testing option")).option.unsafeRunSync() shouldBe None
  }

  "handleErrorWith" should "return value" in {
    forAll { (x: Int) =>
      IO(x).handleErrorWith(_ => IO(x / 0)).unsafeRunSync() shouldEqual x
    }
  }

  "handleErrorWith" should "handle error" in {
    forAll { (x: Int, y:Int) =>
      IO(x / 0).handleErrorWith(_ => IO(y)).unsafeRunSync() shouldEqual y
    }
  }

  "redeem" should "return value" in {
    forAll { (x: Int) =>
      IO(x * 2).redeem(_ => 0, i => i + 1).unsafeRunSync() shouldEqual x * 2 + 1
    }
  }

  "redeem" should "handle error" in {
    forAll { (x: Int, y:Int) =>
      IO[Int](???).redeem(_ => x, _ => y).unsafeRunSync() shouldEqual x
    }
  }

  "redeemWith" should "return value" in {
    forAll { (x: Int, y:Int) =>
      IO(x * 2).redeemWith(_ => IO(0), _ => IO(y)).unsafeRunSync() shouldEqual y
    }
  }

  "redeemWith" should "handle error" in {
    forAll { (x: Int, y:Int) =>
      IO[Int](???).redeemWith(_ => IO(x), _ => IO(y)).unsafeRunSync() shouldEqual x
    }
  }

}
