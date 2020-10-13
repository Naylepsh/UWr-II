package test.scala.figures

import main.scala.figures.{Point, Triangle}
import main.scala.numbers.Rational

object TriangleTests {

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

    val createTriangle = () => {
      new Triangle(points)
      ()
    }

    assertThrow[IllegalArgumentException](createTriangle,
      "Triangle should not allow initialization without all 3 points")
  }

  private def areaTests(): Unit = {
    computeAreaOf3Points()
  }

  private def computeAreaOf3Points(): Unit = {
    val p1 = new Point(Rational(0), Rational(0))
    val p2 = new Point(Rational(0), Rational(2))
    val p3 = new Point(Rational(2), Rational(0))
    val points = Array(p1, p2, p3)
    val triangle = new Triangle(points)

    val area = triangle.area

    assert(area == 2, createErrorMessage("area", points.toList, area))
  }
}
