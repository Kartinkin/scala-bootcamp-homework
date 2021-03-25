package com.evolutiongaming.bootcamp.testing2

import scala.util.Try
import Calculator._

/** Simple calculator with buttons.
  *
  * @param memory whatever is stored in the memory.
  * @param screen whatever you see on the screen.
  */
case class Calculator(
  memory: Int = 0,
  val screen: Either[String, Int] = Right(0),
  operation: Option[Operation] = None
) {

  def enter(button: Button): Calculator = button match {
    case NumPad(digit) =>
      val v = 10 * screen.getOrElse(0)
      this.copy(screen = Right(if (v >= 0) v + digit else v - digit))
    case Operation.C =>
      this.copy(screen = Right(0))
    case Operation.AC =>
      this.copy(memory = 0, screen = Right(0), operation = None)
    case Operation.Equal =>
      this.copy(memory = 0, screen = calculate(), operation = None)
    case Operation.Hundred =>
      this.copy(screen = Right(100 * screen.getOrElse(0)))
    case Operation.Thousand =>
      this.copy(screen = Right(1000 * screen.getOrElse(0)))
    case Operation.Negative =>
      this.copy(screen = screen.map { s => -s })
    case Operation.Percent =>
      this.copy(screen = screen.map { s => s / 100 })
    case Operation.Reverse =>
      this.copy(screen =
        Calculator(memory = 1, screen = screen, operation = Some(Operation.Divide))
        .enter(Operation.Equal)
        .screen)
    case Operation(operation) =>
      calculate() match {
        case err@Left(_) => this.copy(memory = 0, screen = err, operation = None)
        case Right(result) => this.copy(memory = result, screen = Right(0), operation = Some(operation))
      }
  }

  def calculate(): Either[String, Int] = operation match {
    case Some(Operation.Plus) => screen.map { s => memory + s }
    case Some(Operation.Minus) => screen.map { s => memory - s }
    case Some(Operation.Multiply) => screen.map { s => memory * s }
    case Some(Operation.Divide) =>
      Try(screen.map { s => memory / s })
      .getOrElse(Left("Not a number"))
    case default => screen
  }
}
object Calculator {
  sealed trait Button

  sealed trait Operation extends Button
  object Operation {
    def unapply(o: Operation): Option[Operation] = Some(o)
    object Plus extends Operation
    object Minus extends Operation
    object Multiply extends Operation
    object Divide extends Operation
    object C extends Operation
    object AC extends Operation
    object Negative extends Operation
    object Reverse extends Operation
    object Percent extends Operation
    object Hundred extends Operation
    object Thousand extends Operation
    object Equal extends Operation
  }


  sealed class NumPad(val num: Int) extends Button
  object NumPad {
    def unapply(n: NumPad): Option[Int] = Some(n.num)
    object Zero extends NumPad(0)
    object One extends NumPad(1)
    object Two extends NumPad(2)
    object Three extends NumPad(3)
    object Four extends NumPad(4)
    object Five extends NumPad(5)
    object Six extends NumPad(6)
    object Seven extends NumPad(7)
    object Eight extends NumPad(8)
    object Nine extends NumPad(9)
  }
}
