package object Pizzeria {
  trait HasPrice {
    val price: Double
  }

  sealed abstract class Size
  case object Small extends Size
  case object Regular extends Size
  case object Large extends Size

  sealed abstract class Crust
  case object Thin extends Crust
  case object Thick extends Crust

  sealed abstract class Topping(val price: Double) extends HasPrice
  case object Ketchup extends Topping(0.5)
  case object Garlic extends Topping(0.5)

  sealed abstract class Meat(val price: Double) extends HasPrice
  case object Salami extends Meat(1)

  sealed abstract class Drink(val price: Double) extends HasPrice
  case object Lemonade extends Drink(2)

  sealed abstract class Discount
  case object Student extends Discount
  case object Senior extends Discount

  sealed abstract class PizzaType(val price: Double) extends HasPrice
  case object Margarita extends PizzaType(5)
  case object Pepperoni extends PizzaType(6.5)
  case object Funghi extends PizzaType(7)

  case class Pizza(
    pizzaType: PizzaType,
    size: Size,
    crust: Crust,
    extraMeat: Option[Meat] = None,
    extraTopping: Option[Topping] = None) extends HasPrice {

    override def toString: String = {
      val core = s"""type: $pizzaType
         |size: $size
         |crust: $crust
         |""".stripMargin
      val meat = extraMeat.map("meat: " + _ + "\n").getOrElse("")
      val topping = extraTopping.map("topping: " + _ + "\n").getOrElse("")

      "----Pizza----\n" + core + meat + topping + "-------------\n"
    }

    // for whatever reason calling extraNeat.map... doesn't work (same with extraTopping),
    // hence why these seemingly unnecessary helpers
    // Also, Optional.map().sum is shorter than match case Some, case None
    def extraMeatPrice(): Double = extraMeat.map(_.price).sum
    def extraToppingPrice(): Double = extraTopping.map(_.price).sum

    override val price: Double = {
      val coreCost = pizzaType.price
      val meatCost = extraMeatPrice()
      val toppingCost = extraToppingPrice()
      val sizeCostMultiplier = size match {
        case Small => 0.9
        case Regular => 1
        case Large => 1.5
      }

      sizeCostMultiplier * (coreCost + meatCost + toppingCost)
    }
  }
}
