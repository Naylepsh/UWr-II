package Tests
import Pizzeria._
import Orders._

object OrdersTests {
  def runTests(): Unit = {
    testInvalidPhoneNumberThrowsError()
    testCalculatingPriceOfExtraMeat()
    testCalculatingPriceOfPizzas()
    testCalculatingPriceOfPizzasOfType()
    testCalculatingTotalPrice()
    testOrderBeingCheaperWithDiscount()
  }

  def testInvalidPhoneNumberThrowsError(): Unit = {
    try {
      val pizza = Pizza(Margarita, Regular, Thick)
      val _ = new Order(
        "john", "some street 7", "123",
        Some(List(pizza)), Some(List(Lemonade)))
    } catch {
      case _: IllegalArgumentException => ()
      case _: Throwable => assert(assertion = false, "test failed")
    }
  }

  def testCalculatingPriceOfExtraMeat(): Unit = {
    val pizza = Pizza(Margarita, Regular, Thick, Some(Salami))
    val pizza2 = Pizza(Pepperoni, Regular, Thick, Some(Salami))
    val order = new Order(
      "john", "some street 7", "123456789",
      Some(List(pizza, pizza2)), Some(List(Lemonade)))

    val extraMeatPrice = order.extraMeatPrice

    assert(extraMeatPrice.get == pizza.extraMeatPrice() + pizza2.extraMeatPrice())
  }

  def testCalculatingPriceOfPizzas(): Unit = {
    val pizza = Pizza(Margarita, Regular, Thick, Some(Salami))
    val pizza2 = Pizza(Pepperoni, Regular, Thick, Some(Salami))
    val order = new Order(
      "john", "some street 7", "123456789",
      Some(List(pizza, pizza2)), Some(List(Lemonade)))

    val price = order.pizzasPrice

    assert(price.get == pizza.price + pizza2.price)
  }

  def testCalculatingPriceOfPizzasOfType(): Unit = {
    val pizza = Pizza(Margarita, Regular, Thick, Some(Salami))
    val pizza2 = Pizza(Pepperoni, Regular, Thick, Some(Salami))
    val order = new Order(
      "john", "some street 7", "123456789",
      Some(List(pizza, pizza2)), Some(List(Lemonade)))

    val price = order.priceByType(Margarita)

    assert(price.get == pizza.price)
  }

  def testCalculatingTotalPrice(): Unit = {
    val pizza = Pizza(Margarita, Regular, Thick, Some(Salami))
    val order = new Order(
      "john", "some street 7", "123456789",
      Some(List(pizza)), Some(List(Lemonade)))

    val price = order.price

    assert(price == pizza.price + Lemonade.price)
  }

  def testOrderBeingCheaperWithDiscount(): Unit = {
    val pizza = Pizza(Margarita, Regular, Thick, Some(Salami))
    val orderWithoutDiscount = new Order(
      "john", "some street 7", "123456789",
      Some(List(pizza)), Some(List(Lemonade)))
    val orderWithDiscount = new Order(
      "john", "some street 7", "123456789",
      Some(List(pizza)), Some(List(Lemonade)), Some(Senior)
    )


    assert(orderWithoutDiscount.price > orderWithDiscount.price)
  }
}
