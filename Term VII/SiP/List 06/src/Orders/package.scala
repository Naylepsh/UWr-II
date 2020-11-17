import Pizzeria._

package object Orders {
  class Order(
    name: String,
    address: String,
    phone: String,
    pizzas: Option[List[Pizza]],
    drinks: Option[List[Drink]],
    discount: Option[Discount] = None,
    specialInfo: Option[String] = None) {

    require(phone.matches("[0-9]{9}"))

    override def toString: String = {
      val core =
        s"""-----Order-----
           |name: $name
           |address: $address
           |phone $phone
           |""".stripMargin
      val ps = pizzas match {
        case Some(values) => "pizzas:\n" + values.map(_.toString).mkString("")
        case None => ""
      }
      val ds = drinks match {
        case Some(values) => "drinks: " + values.map(_.toString).mkString(", ") + "\n"
        case None => ""
      }
      val disc = discount.map("discount: " + _.toString + "\n").getOrElse("")
      val info = specialInfo.map("special info: " + _).getOrElse("")

      core + ps + ds + disc + info + "---------------"
    }

    def extraMeatPrice: Option[Double] = pizzas match {
      case Some(values) => Some(values.map(_.extraMeatPrice()).sum)
      case None => None
    }

    def pizzasPrice: Option[Double] = pizzas match {
      case Some(values) => Some(values.map(_.price).sum)
      case None => None
    }

    def drinksPrice: Option[Double] = drinks match {
      case Some(values) => Some(values.map(_.price).sum)
      case None => None
    }

    def priceByType(pizzaType: PizzaType): Option[Double] = pizzas match {
      case Some(values) => Some(values.filter(_.pizzaType == pizzaType).map(_.price).sum)
      case None => None
    }

    // For whatever reason calling pizzasPrice.getOrElse(0) doesn't work (same with drinksPrice),
    // hence why these seemingly unnecessary helpers
    private val priceOfPizzas: Double = pizzasPrice.getOrElse(0)
    private val priceOfDrinks: Double = drinksPrice.getOrElse(0)

    val price: Double = {
      val discountMultiplier = discount match {
        case Some(Student) => 0.95
        case Some(Senior) => 0.93
        case None => 1
      }

      discountMultiplier * (priceOfPizzas + priceOfDrinks)
    }
  }
}
