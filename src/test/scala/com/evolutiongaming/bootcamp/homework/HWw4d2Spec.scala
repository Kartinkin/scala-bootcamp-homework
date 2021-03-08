package com.evolutiongaming.bootcamp.implicits

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class TypeclassTaskSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  import TypeclassTask._
  "Any string" should "has hash code" in {
    forAll { (s: String) =>
      s.hash shouldEqual s.map(_.toInt).fold(0) { (acc, v) => acc + v }
    }
  }
}

class Task2Spec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  import Task2._
  "Any user" should "show its name" in {
    forAll { (i: Int, s: String) =>
      User(i, s).show shouldEqual s
    }
  }
}

class Task3Spec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  import Task3._
  "User" should "be parsable" in {
    forAll { (i: String, n: String) =>
      whenever (i.nonEmpty && n.nonEmpty) {
        s"${i}:${n}".parse[User] shouldEqual Right(User(i, n))
      }
    }
  }
  "olala" should "return error" in {
    "lalala".parse[User] shouldEqual Left("Error")
  }
}
