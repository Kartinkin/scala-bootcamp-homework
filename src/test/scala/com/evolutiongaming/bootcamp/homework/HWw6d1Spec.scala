package com.evolutiongaming.bootcamp.typeclass.v2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class QAndAExamplesSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  import QAndAExamples._
  "combineAll" should "combine for non-empty lists" in {
    combineAll(List(1L, 2L, 3L)) shouldEqual 6
  }

  "combineAll" should "combine list with zero" in {
    combineAll(List(1, 2, 3), 0) shouldEqual 6
    combineAll(List(), 1L) shouldEqual 1
  }

  "assocAll" should "combine all lists" in {
    assocAll(List(1, 2, 3)) shouldEqual 6
  }

  "combineAll" should "combine Option" in {
	  combineAll(List(Some(1), None, Some(3))) shouldEqual Some(4)
  	combineAll(List[Option[Int]](None, None)) shouldEqual None
	}

	"assocAll" should "combine Option" in {
  	optionMonoid[Int].combine(None, None) shouldEqual None
  	assocAll(List[Option[String]]()) shouldEqual None
	}

  "combineAll" should "work with function monoid" in {
	  combineAll(List((a: String) => a.length, (a: String) => a.toInt)).apply("123") shouldEqual 126
	}

	"mapN" should "work with Tuple2" in {
	  (Option(1), Option(2)).mapN(_ + _) shouldEqual Some(3)
	  (Option(1), None: Option[Int]).mapN(_ + _) shouldEqual None
	}

	"Semigroupal" should "work for Map" in {
		(Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) shouldEqual Map(2 -> "bc")
	}

	"traverse" should "run" in {
	  traverse(List(1, 2, 3)) { i => Option.when(i % 2 == 1)(i) } shouldEqual None
		traverse(List(1, 2, 3)) { i => Some(i + 1): Option[Int] } shouldEqual Some(List(2, 3, 4))
	}

}
