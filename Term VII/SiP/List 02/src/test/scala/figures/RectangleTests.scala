package test.scala.figures

import main.scala.figures.{Point, Rectangle}
import main.scala.numbers.Rational

object RectangleTests {
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

    val createRectangle = () => {
      new Rectangle(points)
      ()
    }

    assertThrow[IllegalArgumentException](createRectangle,
      "Rectangle should not allow initialization without all 4 points")
  }

  private def areaTests(): Unit = {
    computeAreaOfPointWidthHeightInit()
    computeAreaOfTwoPointsInit()
  }

  private def computeAreaOfPointWidthHeightInit(): Unit = {
    val point = new Point(Rational(1), Rational(1))
    val width = 1
    val height = 2
    val rect = Rectangle(point, width, height)

    val area = rect.area

    assert(area == width * height, createErrorMessage("area", List(point, width, height), area))
  }

  private def computeAreaOfTwoPointsInit(): Unit = {
    val topLeft = new Point(Rational(0), Rational(1))
    val bottomRight = new Point(Rational(1), Rational(0))
    val rect = Rectangle(topLeft, bottomRight)

    val area = rect.area

    assert(area == 1, createErrorMessage("area", List(topLeft, bottomRight), area))
  }
}
