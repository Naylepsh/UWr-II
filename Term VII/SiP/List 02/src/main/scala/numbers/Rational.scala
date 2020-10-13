package main.scala.numbers

import main.scala.numbers.Rational.gcd
import scala.annotation.tailrec

class Rational(val numerator: Int, val denominator: Int) {
  require(denominator != 0, "Denominator cannot be 0")

  def +(other: Rational): Rational = {
    val numerator = this.numerator * other.denominator + other.numerator * this.denominator
    val denominator = this.denominator * other.denominator

    Rational(numerator, denominator)
  }

  def -(other: Rational): Rational = {
    val negated = new Rational(-other.numerator, other.denominator)

    this + negated
  }

  def *(other: Rational): Rational = {
    val numerator = this.numerator * other.numerator
    val denominator = this.denominator * other.denominator

    Rational(numerator, denominator)
  }

  def /(other: Rational): Rational = {
    val inverted = new Rational(other.denominator, other.numerator)

    this * inverted
  }

  override def toString: String = {
    if (this.numerator == 0) return "0"

    val integer: Int = this.numerator / this.denominator
    val divisor = gcd(this.numerator, this.denominator)
    val simplifiedDenominator = this.denominator / divisor
    val simplifiedNumerator = this.numerator / divisor - (integer * simplifiedDenominator)

    RationalPrettifier.toPrettyString(integer, simplifiedNumerator, simplifiedDenominator)
  }

  def toDouble: Double = {
    this.numerator.toDouble / this.denominator
  }
}

object Rational {
  val defaultDenominator = 1
  val zero: Rational = Rational(0)
  val one: Rational = Rational(1)

  @tailrec
  def gcd(a: Int, b: Int ): Int = if (b == 0) a else gcd(b, a % b)

  def apply(numerator: Int, denominator: Int = defaultDenominator): Rational = {
    val divisor = gcd(numerator, denominator)

    new Rational(numerator / divisor, denominator / divisor)
  }
}

private[numbers] object RationalPrettifier {
  def toPrettyString(integer: Int, numerator: Int, denominator: Int): String = {
//    not universally true, but works with current Rational implementation
    val isNegative = numerator < 0 || denominator < 0 || integer < 0

    val sign = if (isNegative) "-" else ""
    val integerPart = prettyInteger(integer, numerator)
    val fractionPart = prettyFraction(numerator, denominator)

    sign + integerPart + fractionPart
  }

  private def prettyInteger(integer: Int, numerator: Int): String = {
    import math.abs

    val absInteger = abs(integer)
    if (integer == 0 && numerator != 0) return ""

    absInteger.toString
  }

  private def prettyFraction(numerator: Int, denominator: Int) = {
    import Math.abs

    val absNumerator = abs(numerator)
    val absDenominator = abs(denominator)

    if (absNumerator == 0) "" else absNumerator + "/" + absDenominator
  }
}
