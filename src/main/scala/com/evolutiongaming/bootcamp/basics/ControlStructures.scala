package com.evolutiongaming.bootcamp.basics

import scala.io.Source

object ControlStructures {

  sealed trait Command {
    def evaluate: Either[ErrorMessage, Result]
  }

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command {
      override def evaluate: Either[ErrorMessage, Result] = (dividend, divisor) match {
        case (_, 0) => Left(ErrorMessage("Error: division by zero"))
        case (x, y) => Right(Result(Divide(x, y), x / y))
      }
    }

    final case class Sum(numbers: List[Double]) extends Command {
      override def evaluate: Either[ErrorMessage, Result] = numbers match {
        case Nil => Left(ErrorMessage("Error: the list is empty"))
        case x => Right(Result(Sum(x), x.sum))
      }
    }

    final case class Average(numbers: List[Double]) extends Command {
      override def evaluate: Either[ErrorMessage, Result] = numbers match {
        case Nil => Left(ErrorMessage("Error: the list is empty"))
        case x => Right(Result(Average(x), x.sum / x.length))
      }
    }

    final case class Min(numbers: List[Double]) extends Command {
      override def evaluate: Either[ErrorMessage, Result] = numbers match {
        case Nil => Left(ErrorMessage("Error: the list is empty"))
        case x => Right(Result(Min(x), x.min))
      }
    }

    final case class Max(numbers: List[Double]) extends Command {
      override def evaluate: Either[ErrorMessage, Result] = numbers match {
        case Nil => Left(ErrorMessage("Error: the list is empty"))
        case x => Right(Result(Max(x), x.max))
      }
    }

  }

  final case class ErrorMessage(value: String)

  final case class Result(command: Command, result: Double) {
    def formatted: String = command match {
      case Divide(x, y) => s"$x divided by $y is $result"
      case Sum(list) => s"the sum of ${list.mkString(" ")} is $result"
      case Average(list) => s"the average of ${list.mkString(" ")} is $result"
      case Min(list) => s"the minimum of ${list.mkString(" ")} is $result"
      case Max(list) => s"the maximum of ${list.mkString(" ")} is $result"
    }
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    x.split("\\s+").toList match {
      case "divide" :: a :: b :: Nil => Right(Divide(a.toDouble, b.toDouble))
      case "sum" :: xs => Right(Sum(xs.map(_.toDouble)))
      case "average" :: xs => Right(Average(xs.map(_.toDouble)))
      case "min" :: xs => Right(Min(xs.map(_.toDouble)))
      case "max" :: xs => Right(Max(xs.map(_.toDouble)))
      case _ => Left(ErrorMessage("Error: invalid command"))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = x.evaluate

  def renderResult(x: Result): String = x.formatted

  def process(x: String): String = {
    val result = for {
      parsedCommand <- parseCommand(x)
      calculatedResult <- calculate(parsedCommand)
    } yield calculatedResult

    result match {
      case Right(x) => renderResult(x);
      case Left(e) => e.value;
    }
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
