package com.evolutiongaming.bootcamp

import com.evolutiongaming.bootcamp.Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BasicsSpec extends AnyFlatSpec {
  "gcd" should "correctly calculate Greatest common divisor" in {
    gcd(12, 8) shouldEqual Some(4)
    gcd(8, 12) shouldEqual Some(4)
    gcd(2, 2) shouldEqual Some(2)
    gcd(1, 3) shouldEqual Some(1)
    gcd(3, 1) shouldEqual Some(1)
    gcd(0, 4) shouldEqual Some(4)
    gcd(0, 0) shouldEqual None
  }

  "lcm" should "correctly calculate Lowest common denominator" in {
    lcm(12, 8) shouldEqual Some(24)
    lcm(8, 12) shouldEqual Some(24)
    lcm(2, 2) shouldEqual Some(2)
    lcm(1, 3) shouldEqual Some(3)
    lcm(3, 1) shouldEqual Some(3)
    lcm(0, 4) shouldEqual Some(0)
    lcm(0, 0) shouldEqual None
  }
}
