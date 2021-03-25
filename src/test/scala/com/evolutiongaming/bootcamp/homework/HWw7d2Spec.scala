package com.evolutiongaming.bootcamp.testing2

import Calculator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class HWw7d2Spec extends AnyFreeSpec {

  trait PressButtons[T] {
    def enterNumber(entity: T, number: Int): T
  }
  object PressButtons {
    def apply[T: PressButtons]: PressButtons[T] = implicitly
  }
  implicit class PressButtonsSyntax[T: PressButtons](x: T) {
    def enterNumber(n: Int): T =
      PressButtons[T].enterNumber(x, n)
  }

  implicit val CalculatorRandomNumber: PressButtons[Calculator] = { (calc, num) =>
    var c = calc
    for (n <- num.toString.map(_.asDigit)) {
      c = c.enter(new NumPad(n))
    }
    c
  }

  val r = new scala.util.Random()
  def randomNumber: Int = r.nextInt(Integer.MAX_VALUE)

  "Digit buttons should work" in {
    val n = randomNumber
    val c = new Calculator()
      .enterNumber(n)
    c shouldEqual Calculator(0, Right(n))
  }

  "00 button multiply by 100" in {
    val n = randomNumber
    val c = new Calculator()
      .enterNumber(n)
      .enter(Operation.Hundred)
    c shouldEqual Calculator(0, Right(n * 100))
  }

  "C clears screen" in {
    val n = randomNumber
    val c = new Calculator()
      .enterNumber(n)
      .enter(Operation.Plus)
      .enterNumber(randomNumber)
      .enter(Operation.C)
    c shouldEqual Calculator(n, Right(0), Some(Operation.Plus))
  }

  "AC clears all" in {
    val c = new Calculator()
      .enterNumber(randomNumber)
      .enter(Operation.Plus)
      .enterNumber(randomNumber)
      .enter(Operation.AC)
    c shouldEqual Calculator()
  }

  "Sign button makes a number negative" in {
    val n = randomNumber
    val c = new Calculator()
      .enterNumber(n)
      .enter(Operation.Negative)
    c shouldEqual Calculator(0, Right(-n))
  }

  "Can press sign button in the middle" in {
    val n = r.nextInt(Integer.MAX_VALUE >> 9)
    val c = new Calculator()
      .enterNumber(n)
      .enter(Operation.Negative)
      .enterNumber(42)
    c shouldEqual Calculator(0, Right(- (n * 100 + 42)))
  }

  "No changes if press single button twice" in {
    val n = randomNumber
    val c = new Calculator()
      .enterNumber(n)
      .enter(Operation.Negative)
      .enter(Operation.Negative)
    c shouldEqual Calculator(0, Right(n))
  }

  "Sum two numbers" in {
    val n1 = randomNumber
    val n2 = randomNumber
    val c = new Calculator()
      .enterNumber(n1)
      .enter(Operation.Plus)
      .enterNumber(n2)
      .enter(Operation.Equal)
    c shouldEqual Calculator(0, Right(n1 + n2))
  }

  "Operation sequence" in {
    val n1 = randomNumber
    val n2 = randomNumber
    val n3 = randomNumber
    val n4 = randomNumber
    val c = new Calculator()
      .enterNumber(n1)
      .enter(Operation.Multiply)
      .enterNumber(n2)
      .enter(Operation.Plus)
      .enterNumber(n3)
      .enter(Operation.Minus)
      .enterNumber(n4)
      .enter(Operation.Equal)
    c shouldEqual Calculator(0, Right(n1 * n2 + n3 - n4))
  }

  "Divide by zero gives error" in {
    Calculator().enter(Operation.Reverse).screen should be ('left)

    val c = new Calculator()
      .enterNumber(randomNumber)
      .enter(Operation.Divide)
      .enterNumber(0)
      .enter(Operation.Equal)
    c.screen should be ('left)
  }

  "Operation sequence with error" in {
    val c = new Calculator()
      .enterNumber(randomNumber)
      .enter(Operation.Divide)
      .enterNumber(0)
      .enter(Operation.Plus)
    c.screen should be ('left)
  }

}
