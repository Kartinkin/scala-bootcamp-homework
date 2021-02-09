package com.evolutiongaming.bootcamp.basics

import scala.io.Source

object ControlStructuresHomework {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  def strToDouble(number: String): Either[ErrorMessage, Double] =
    number.toDoubleOption.toRight(ErrorMessage(s"Bad numberic format: ${number}"))

  def strsToDoubles(numbers: List[String]): Either[ErrorMessage, List[Double]] = {
    val (errors, doubles) = numbers.partitionMap(s => strToDouble(s))
    errors.headOption.toLeft(doubles)
  }

  sealed trait Command
  object Command {
    final case class Divide private(dividend: Double, divisor: Double) extends Command
    object Divide {
      def apply(dividend: String, divisor: String): Either[ErrorMessage, Divide] =
        (for {
          x <- strToDouble(dividend)
          y <- strToDouble(divisor)
        } yield Divide(x,y)) match {
          case e: Left[ErrorMessage, Divide] => e
          case Right(d@Divide(x, y)) => Either.cond(y != 0, d, ErrorMessage("Dude, can't divide by zero"))
        }
     }

    final case class Sum private(numbers: List[Double]) extends Command
    object Sum {
      def apply(numbers: List[String]): Either[ErrorMessage, Sum] =
        for {
          doubles <- strsToDoubles(numbers)
        } yield Sum(doubles)
    }

    final case class Average private(numbers: List[Double]) extends Command
    object Average {
      def apply(numbers: List[String]): Either[ErrorMessage, Average] =
        for {
          doubles <- strsToDoubles(numbers)
        } yield Average(doubles)
    }

    final case class Min private(numbers: List[Double]) extends Command
    object Min {
      def apply(numbers: List[String]): Either[ErrorMessage, Min] =
        for {
          doubles <- strsToDoubles(numbers)
        } yield Min(doubles)
    }

    final case class Max private(numbers: List[Double]) extends Command
    object Max {
      def apply(numbers: List[String]): Either[ErrorMessage, Max] =
        for {
          doubles <- strsToDoubles(numbers)
        } yield Max(doubles)
    }
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  object Result {
    final case class Divide(command: Command.Divide, result: Double) extends Result
    final case class Sum(command: Command.Sum, result: Double) extends Result
    final case class Average(command: Command.Average, result: Double) extends Result
    final case class Min(command: Command.Min, result: Double) extends Result
    final case class Max(command: Command.Max, result: Double) extends Result
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    import Command._

    x.split("\\s+").toList match {
      case "divide" :: x :: y :: Nil => Divide(x, y)
      case "sum" :: numbers => Sum(numbers)
     case "average" :: numbers => Average(numbers)
     case "min" :: numbers => Min(numbers)
     case "max" :: numbers => Max(numbers)
     case _ => Left(ErrorMessage("Unlnow command"))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = x match {
    case c@Command.Divide(x, y) => Right(Result.Divide(c, x / y))
    case c@Command.Sum(l) => Right(Result.Sum(c, l.sum))
    case c@Command.Average(l) => Right(Result.Average(c, l.sum / l.length))
    case c@Command.Min(l) => Right(Result.Min(c, l.min))
    case c@Command.Max(l) => Right(Result.Max(c, l.max))
  }

  def renderResult(x: Result): String = {
    import Result._
    x match {
      case Divide(Command.Divide(x, y), r) => s"${x} divided by ${y} is ${r}"
      case Sum(Command.Sum(l), r) => s"the sum of ${l.mkString(" ")} is ${r}"
      case Average(Command.Average(l), r) => s"the average of ${l.mkString(" ")} is ${r}"
      case Max(Command.Max(l), r) => s"the maximum of ${l.mkString(" ")} is ${r}"
      case Min(Command.Min(l), r) => s"the minimum of ${l.mkString(" ")} is ${r}"
    }
  }

  def process(x: String): String =
    (for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield result)
    .fold(error => s"Error: ${error.value}",
          result => renderResult(result))


  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
