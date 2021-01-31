package com.evolutiongaming.bootcamp

import scala.annotation.tailrec

object Basics {
  def lcm(a: Int, b: Int): Option[Int] = gcd(a, b).map(gcd => Math.abs(a * b / gcd))

  @tailrec
  def gcd(a: Int, b: Int): Option[Int] = (a, b) match {
    case (0, 0) => None
    case (a, 0) => Some(Math.abs(a))
    case (a, b) => gcd(b, a % b)
  }
}
