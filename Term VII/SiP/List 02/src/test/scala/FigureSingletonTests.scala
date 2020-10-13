package test.scala

import main.scala.FigureSingleton
import main.scala.figures.{Point, Rectangle, Square, Triangle}
import main.scala.numbers.Rational


object FigureSingletonTests {
  def runTests(): Unit = {
    areaOfMultipleOfTheSameFigure()
    areaOfMultipleDifferentFigures()
  }

  private def areaOfMultipleOfTheSameFigure(): Unit = {
    val point = new Point(Rational(1), Rational(1))
    val size = 2
    val square = Square(point, size)
    val figures = List(square, square, square)

    val areaSum = FigureSingleton.areaSum(figures)
    val expectedAreaSum = figures.length * square.area

    assert(areaSum == expectedAreaSum,
      "Area of multiple of the same figure has not been computed properly. " +
        s"Expected $expectedAreaSum, got $areaSum")
  }

  private def areaOfMultipleDifferentFigures(): Unit = {
    val point1 = new Point(Rational(0), Rational(0))
    val square = Square(point1, 1)
    val rectangle = Rectangle(point1, 1, 1)
    val point2 = new Point(Rational(1), Rational(2))
    val point3 = new Point(Rational(2), Rational(3))
    val point4 = new Point(Rational(4), Rational(5))
    val triangle = new Triangle(Array(point2, point3, point4))
    val figures = List(square, rectangle, triangle)

    val expectedAreaSum = square.area + rectangle.area + triangle.area
    val areaSum = FigureSingleton.areaSum(figures)

    assert(areaSum == expectedAreaSum,
      "Area of multiple different figures has not been computed properly. " +
        s"Expected $expectedAreaSum, got $areaSum")
  }
}
