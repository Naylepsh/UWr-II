package test.scala.numbers

import main.scala.numbers.Rational

object RationalTests {
  import test.scala.helpers.Err.{assertThrow, createErrorMessage}

  def runTests(): Unit = {
    initializationTests()
    additionTests()
    subtractionTests()
    multiplicationTest()
    divisionTests()
  }

  private def initializationTests(): Unit = {
    invalidDenominatorTest()
    initializeZeroTest()
    initializeOneTest()
  }

  private def invalidDenominatorTest(): Unit = {
    val invalidConstructorArgs = () => {
      new Rational(0, 0)
      ()
    }

    assertThrow[IllegalArgumentException](invalidConstructorArgs,
      "Constructor did not throw illegal arg exception")
  }

  private def initializeZeroTest(): Unit = {
    val r = Rational.zero

    assert(r.numerator == 0, "Zero factory did not provide rational equal to zero")
  }

  private def initializeOneTest(): Unit = {
    val r = Rational.one

    assert(r.numerator == 1, "Zero factory did not provide rational equal to zero")
  }

  private def additionTests(): Unit = {
    basicAdditionTest()
    addNegativeRationalTest()
  }

  private def basicAdditionTest(): Unit = {
    val r1 = Rational (1, 2)
    val r2 = Rational (1, 2)
    val res = r1 + r2
    assert (res.numerator == 1 && res.denominator == 1,
      createErrorMessage("+", List(r1, r2), res))
  }

  private def addNegativeRationalTest(): Unit = {
    val r1 = Rational (1, 2)
    val r2 = Rational (-1, 2)

    val res = r1 + r2

    assert (res.numerator == 0 && res.denominator == 1,
      createErrorMessage("+", List(r1, r2), res))
  }

  private def subtractionTests(): Unit = {
    basicSubtractionTest()
  }

  private def basicSubtractionTest(): Unit = {
    val r1 = Rational (1, 2)
    val r2 = Rational (1, 2)

    val res = r1 - r2

    assert (res.numerator == 0,
      createErrorMessage("-", List(r1, r2), res))
  }

  private def multiplicationTest(): Unit = {
    basicMultiplicationTest()
    negativeAndPositiveMultiplication()
    doubleNegativeMultiplicationTest()
  }

  private def basicMultiplicationTest(): Unit = {
    val r1 = Rational(1, 2)

    val res = r1 * r1

    assert(res.numerator == 1 && res.denominator == 4,
      createErrorMessage("*", List(r1, r1), res))
  }

  private def negativeAndPositiveMultiplication(): Unit = {
    val r1 = Rational(1, 2)
    val r2 = Rational(-1, 2)

    val res = r1 * r2

    assert(res.toString == "-1/4",
      createErrorMessage("*", List(r1, r2), res))
  }

  private def doubleNegativeMultiplicationTest(): Unit = {
    val r1 = Rational(-1, 2)

    val res = r1 * r1

    assert(res.numerator == 1 && res.denominator == 4,
      createErrorMessage("*", List(r1, r1), res))
  }

  private def divisionTests(): Unit = {
    basicDivisionTest()
    negativeAndPositiveDivisionTest()
    doubleNegativeDivisionTest()
  }

  private def basicDivisionTest(): Unit = {
    val r1 = Rational(1, 2)

    val res = r1 / r1

    assert(res.numerator == 1 && res.denominator == 1,
      createErrorMessage("/", List(r1, r1), res))
  }

  private def negativeAndPositiveDivisionTest(): Unit = {
    val r1 = Rational(1, 2)
    val r2 = Rational(-1, 2)

    val res = r1 / r2

    assert(res.toString == "-1",
      createErrorMessage("/", List(r1, r2), res))
  }

  private def doubleNegativeDivisionTest(): Unit = {
    val r1 = Rational(-1, 2)

    val res = r1 / r1

    assert(res.toString == "1",
      createErrorMessage("/", List(r1, r1), res))
  }
}
