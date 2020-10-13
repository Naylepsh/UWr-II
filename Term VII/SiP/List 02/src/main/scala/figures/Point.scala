package main.scala.figures
import main.scala.numbers.Rational

class Point(val x: Rational, val y: Rational) {
  override def toString: String = {
    s"(${this.x}, ${this.y})"
  }
}
