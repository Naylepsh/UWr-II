import Orders.Order
import Tests._
import Pizzeria._

object Main extends App {
  def printSamplePizza(): Unit = {
    val pizza = Pizza(Margarita, Regular, Thick, Some(Salami), Some(Ketchup))
    println(pizza.toString)
  }

  def printSampleOrder(): Unit = {
    val pizza = Pizza(Margarita, Regular, Thick)
    val pizza2 = Pizza(Pepperoni, Regular, Thick, Some(Salami))
    val order = new Order(
      "john", "some street 7", "123456789",
      Some(List(pizza, pizza2)), Some(List(Lemonade)), Some(Senior))
    println(order.toString)
  }

  PizzeriaTests.runTests()
  OrdersTests.runTests()
  printSamplePizza()
  printSampleOrder()
}

