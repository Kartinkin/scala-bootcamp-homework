package com.evolutiongaming.bootcamp.basics

import HWw1d2._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HWw1d2Spec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "Vector" should "be commutative" in {
    forAll { (x1: Int, y1: Int, x2: Int, y2: Int) =>
      LinVector(x1, y1) + LinVector(x2, y2) shouldEqual LinVector(x2, y2) + LinVector(x1, y1)
    }
  }

  "Vector" should "have length" in {
    forAll { (x: Double, y: Double, z: Double) =>
      LinVector(x, y, z).mod shouldEqual Math.sqrt(x * x + y * y + z * z)
    }
  }

  "Point" should "be movable in 2d space" in {
    forAll { (x: Double, y: Double, dx: Double, dy: Double) =>
      Point(LinVector(x, y)).move(LinVector(dx, dy)).coord shouldEqual Point(LinVector(x + dx, y + dy)).coord
    }
  }

  "Point" should "be movable in 3d space" in {
    forAll { (x: Double, y: Double, z: Double, dx: Double, dy: Double, dz: Double) =>
      Point(LinVector(x, y, z)).move(LinVector(dx, dy, dz)).coord shouldEqual Point(LinVector(x + dx, y + dy, z + dz)).coord
    }
  }

  "Circle" should "be movable" in {
    forAll { (x: Double, y: Double, dx: Double, dy: Double, r: Double) =>
      whenever (r > 0) {
        (Circle(LinVector(x, y), r).move(LinVector(dx, dy)).coord
          shouldEqual
          Circle(LinVector(x + dx, y + dy), r).coord)
      }
    }
  }

  "Circle" should "be correct" in {
    forAll { (x: Double, y: Double, dx: Double, dy: Double, r: Double) =>
      whenever (r > 0) {
        val circle = Circle(LinVector(x, y), r)
        circle.min shouldEqual LinVector(x - r, y - r)
        circle.max shouldEqual LinVector(x + r, y + r)
      }
    }
  }

  "Circle" should "have area = Pi * r^2" in {
    forAll { (x: Double, y: Double, r: Double) =>
      whenever (r > 0) {
        Circle(LinVector(x, y), r).area shouldEqual Math.PI * r * r
      }
    }
  }

  "Square" should "have area = a^2" in {
    forAll { (x: Double, y: Double, s: Double) =>
      whenever (s > 0) {
        Square(LinVector(x, y), s).area shouldEqual s * s
      }
    }
  }

  "Triangle" should "be movable" in {
    forAll { (x1: Double, y1: Double, x3: Double, y3: Double, dx: Double, dy: Double) =>

      (new Triangle(LinVector(x1, y1), LinVector(y1, x3), LinVector(x3, y3)).move(LinVector(dx, dy)).coord
        shouldEqual
        new Triangle(LinVector(x1 + dx, y1 + dy), LinVector(y1 + dx, x3 + dy), LinVector(x3 + dx, y3 + dy)).coord)

      (new Triangle(LinVector(x1, y1), LinVector(y3, x1), LinVector(x3, y3)).move(LinVector(dx, dy)).vertice3
        shouldEqual
        new Triangle(LinVector(x1 + dx, y1 + dy), LinVector(y3 + dx, x1 + dy), LinVector(x3 + dx, y3 + dy)).vertice3)
    }
  }

  "Cube" should "be movable" in {
    forAll { (x: Double, y: Double, z: Double, dx: Double, dy: Double, dz: Double) =>
      Cube(LinVector(x, y, z), dx.abs).move(LinVector(dx, dy, dz)).coord shouldEqual Cube(LinVector(x + dx, y + dy, z + dz), dx.abs).coord
    }
  }

  "Cube" should "have volume = s^3" in {
    forAll { (x: Double, y: Double, z: Double, s: Double) =>
      whenever (s > 0) {
        Cube(LinVector(x, y, z), s).volume shouldEqual s * s * s
      }
    }
  }

  "Triangle3d" should "have area" in {
    BigDecimal(new Triangle3d(Origin3d, LinVector(0, 1, 0), LinVector(1, 0, 0)).area).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble shouldEqual 0.5
  }

  "Triangle3d" should "be correct" in {
    val t = new Triangle3d(LinVector(-20, 20, 200), LinVector(40, 30, 10), LinVector(300, -500, -100))
    t.min shouldEqual LinVector(-20, -500, -100)
    t.max shouldEqual LinVector(300, 30, 200)
  }

}
