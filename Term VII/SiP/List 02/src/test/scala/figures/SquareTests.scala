package test.scala.figures

import main.scala.figures.{Point, Square}
import main.scala.numbers.Rational

object SquareTests {
  import test.scala.helpers.Err.{assertThrow, createErrorMessage}

  def runTest(): Unit = {
    initializationTests()
    areaTests()
  }

  private def initializationTests(): Unit = {
    initializeWithoutAllPoints()
  }

  private def initializeWithoutAllPoints(): Unit = {
    val point = new Point(Rational(1), Rational(1))
    val points = Array[Point](point)

    val createSquare = () => {
      new Square(points)
      ()
    }

    assertThrow[IllegalArgumentException](createSquare,
      "Square should not allow initialization without all 4 points")
  }

  private def areaTests(): Unit = {
    computeAreaOfPointWidthHeightInit()
    computeAreaOfTwoPointsInit()
  }

  private def computeAreaOfPointWidthHeightInit(): Unit = {
    val point = new Point(Rational(1), Rational(1))
    val size = 2
    val square = Square(point, size)

    val area = square.area

    assert(area == size * size, createErrorMessage("area", List(point, size), area))
  }

  private def computeAreaOfTwoPointsInit(): Unit = {
    val topLeft = new Point(Rational(0), Rational(1))
    val bottomRight = new Point(Rational(1), Rational(0))
    val square = Square(topLeft, bottomRight)

    val area = square.area

    assert(area == 1, createErrorMessage("area", List(topLeft, bottomRight), area))
  }
}
