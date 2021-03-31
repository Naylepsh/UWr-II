package main.scala

import main.scala.figures.Shape

object FigureSingleton {
  def areaSum(figures: List[Shape]): Double = {
    figures.map(_.area).sum
  }

  def printAll(figures: List[Shape]): Unit = {
    for (figure <- figures) println(figure.description)
  }
}
