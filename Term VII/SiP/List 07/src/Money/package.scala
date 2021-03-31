package object Money {
  import scala.language.implicitConversions

  sealed trait Currency
  case object USD extends Currency
  case object EUR extends Currency
  case object PLN extends Currency

  //SIP better to define this hardcoded-val outside of Money.scala (in Test.scala)
  //Thanks to this when Rates will change, user could update these values without modifying your code
  val conversion: Map[(Currency, Currency), BigDecimal] = Map(
    //SIP These (X,X) -> are redundant, better to have some logic to cover this
    //Otherwise, in the future, for each new currency we would need to add an extra line here
    (USD, USD) -> 1,
    (USD, EUR) -> 0.84,
    (USD, PLN) -> 3.76,
    (EUR, USD) -> 1.19,
    (EUR, EUR) -> 1,
    (EUR, PLN) -> 4.46,
    (PLN, USD) -> 0.27,
    (PLN, EUR) -> 0.22,
    (PLN, PLN) -> 1, //SIP Redundant comma - ,
  )

  sealed abstract class CurrencySymbol(val currency: Currency)
  case object $ extends CurrencySymbol(USD)
//  having to copy-paste euro symbol every time one wanted to make an operation on euro feels kinda weird,
//  hence why I'll use E instead
  //SIP Could be also like: case object `€` extends CurrencySymbol(EUR)
  //also could be:
  // val $ = USD
  // val `€` = EUR
  // val zł = PLN
  //Thanks to this CurrencyConverter.convert(from: CurrencySymbol, to: CurrencySymbol) will be redundant
  case object E extends CurrencySymbol(EUR)
  case object zl extends CurrencySymbol(PLN)

  implicit def symbolToCurrencyImplicit(symbol: CurrencySymbol): Currency = symbol.currency

  case class CurrencyConverter(conversion: Map[(Currency, Currency), BigDecimal]) {
    def convert(from: Currency, to: Currency): BigDecimal = conversion((from, to))
    def convert(from: CurrencySymbol, to: CurrencySymbol): BigDecimal = conversion((from.currency, to.currency))
  }


  case class Money(amount: BigDecimal, currency: Currency)(implicit currencyConverter: CurrencyConverter) {
    def as(newCurrency: Currency): Money = {
      Money(amount * currencyConverter.conversion(currency, newCurrency), newCurrency)
    }

    def +(other: Money): Money = Money(amount + (other as currency).amount, currency)

    def -(other: Money): Money = Money(amount - (other as currency).amount, currency)

    def *(multiplier: Double): Money = Money(amount * multiplier, currency)

    def >(other: Money): Boolean = amount > (other as currency).amount

    def <(other: Money): Boolean = amount < (other as currency).amount

    def ==(other: Money): Boolean = amount == (other as currency).amount

    //SIP nice
    def !=(other: Money): Boolean = ! ==(other)

    def >=(other: Money): Boolean = ! <(other)

    def <=(other: Money): Boolean = ! >(other)
  }

  implicit def numberToMoney(amount: Double): Currency => Money = (currency: Currency) => {
    Money(amount, currency)(CurrencyConverter(conversion))
  }
}
