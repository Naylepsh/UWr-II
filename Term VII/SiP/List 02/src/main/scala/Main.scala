package main.scala

import test.scala.FigureSingletonTests
import test.scala.figures.{RectangleTests, SquareTests, TriangleTests}
import test.scala.numbers.RationalTests

object Main extends App {
  RationalTests.runTests()
  RectangleTests.runTest()
  SquareTests.runTest()
  TriangleTests.runTest()
  FigureSingletonTests.runTests()
}
