package main.scala.figures

import main.scala.figures.Rectangle.getVertices

// since Square is immutable,
// the whole "square cannot inherit from rectangle due to setWith/setHeight" problem goes away
// (I think?)
//SIP yes
class Square(points: Array[Point]) extends Rectangle(points) {
  //SIP Validation needed to check if provided points form a Square.
  //For example this is invalid definition - new Square(Array(p1,p1, p2,p2))

  override val description: String = "Square"
}

object Square {
  def apply(topLeft: Point, size: Int) = {
    val width = size
    val height = size
    val points = Rectangle.getVertices(topLeft, width, height)

    new Square(points)
  }

  def apply(topLeft: Point, bottomRight: Point): Rectangle = {
    val points = getVertices(topLeft, bottomRight)

    new Square(points)
  }
}
