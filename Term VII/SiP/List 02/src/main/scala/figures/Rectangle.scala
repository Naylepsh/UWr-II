package main.scala.figures

class Rectangle(val points: Array[Point]) extends Shape(points) {
  require(points.length == 4)
  override val description: String = "Rectangle"

  override def area: Double = {
    val topLeft = points(0)
    val topRight = points(1)
    val bottomLeft = points(2)
    val bottomRight = points(3)

    val triangle1 = new Triangle(Array(topLeft, topRight, bottomLeft))
    val triangle2 = new Triangle(Array(bottomLeft, topRight, bottomRight))

    triangle1.area + triangle2.area
  }
}

object Rectangle {
  import main.scala.numbers.Rational

  def getVertices(topLeft: Point, width: Int, height: Int): Array[Point] = {
    val topRight = new Point(topLeft.x + Rational(width), topLeft.y)
    val bottomLeft = new Point(topLeft.x, topLeft.y - Rational(height))
    val bottomRight = new Point(topRight.x, bottomLeft.y)

    Array(topLeft, topRight, bottomLeft, bottomRight)
  }

  def getVertices(topLeft: Point, bottomRight: Point): Array[Point] = {
    val topRight = new Point(bottomRight.x, topLeft.y)
    val bottomLeft = new Point(topLeft.x, bottomRight.y)

    Array(topLeft, topRight, bottomLeft, bottomRight)
  }

  def apply(topLeft: Point, width: Int, height: Int): Rectangle = {
    val points = getVertices(topLeft, width, height)

    new Rectangle(points)
  }

  def apply(topLeft: Point, bottomRight: Point): Rectangle = {
    val points = getVertices(topLeft, bottomRight)

    new Rectangle(points)
  }
}
