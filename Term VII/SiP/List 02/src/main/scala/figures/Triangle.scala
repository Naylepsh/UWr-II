package main.scala.figures
import main.scala.numbers.Rational

class Triangle(val points: Array[Point]) extends Shape(points) {
  require(points.length == 3)
  override val description = "Triangle"

  override def area: Double = {
    val a = points(0)
    val b = points(1)
    val c = points(2)

    Math.abs(((a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y)) / Rational(2)).toDouble)
  }
}

object Triangle {
  def apply(p1: Point, p2: Point, p3: Point): Triangle = {
    new Triangle(Array(p1, p2, p3))
  }

  def apply(points: Array[Point]): Triangle = {
    new Triangle(points)
  }
}
