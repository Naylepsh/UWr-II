package main.scala.figures

abstract class Shape(points: Array[Point]) {
  def area: Double
  val description: String
}
