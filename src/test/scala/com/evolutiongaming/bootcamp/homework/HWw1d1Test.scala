package com.evolutiongaming.bootcamp.homework

import HWw1d1._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HWw1d1Spec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "gcd" should "return the same as BigInt.gcd" in {
    forAll { (a: Int, b: Int) =>
      whenever (a != Integer.MIN_VALUE && b != Integer.MIN_VALUE) {
        gcd(a, b) shouldEqual BigInt(a).gcd(BigInt(b))
      }
    }
  }

  "gcd" should "be commutative" in {
    forAll { (a: Int, b: Int) =>
      gcd(a, b) shouldEqual gcd(b, a)
    }
  }

  "gcd" should "be n for (n,n)" in {
    forAll { (a: Int) =>
      whenever (a > 0) {
        gcd(a, a) shouldEqual a
      }
    }
  }

  "lcm" should "return lcm" in {
    lcm(24, 42) shouldEqual 168
    lcm(0, 42) shouldEqual 0
    lcm(9, 12) shouldEqual 36
    lcm(36, 48) shouldEqual 144
  }

  "lcm" should "be commutative" in {
    forAll { (a: Int, b: Int) =>
      lcm(a, b) shouldEqual lcm(b, a)
    }
  }

  "lcm" should "be n for (n,n)" in {
    forAll { (a: Int) =>
      whenever (a > 0) {
        lcm(a, a) shouldEqual a
      }
    }
  }

}
