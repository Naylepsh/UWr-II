import Money._

object Main extends App {
  def testAddition(): Unit = {
    testAdditionDollarAndEuro()
    testAdditionZlotyAndDollar()
    testAdditionAndSymbolsZlotyAndDollar()
  }

  def testAdditionDollarAndEuro(): Unit = {
    val usd = 100.01(USD)
    val euro = 200(EUR)

    val total = usd + euro

    val expected = usd.amount +  euro.amount * conversion((EUR, USD))
    assert(total.currency == USD)
    assert(total.amount == expected)
  }

  def testAdditionZlotyAndDollar(): Unit = {
    val zloty = 100.01(zl)
    val dollar = 200($)

    val total = zloty + dollar

    val expected = zloty.amount + dollar.amount * conversion((USD, PLN))
    assert(total.currency == PLN)
    assert(total.amount == expected)
  }

  def testAdditionAndSymbolsZlotyAndDollar(): Unit = {
    val zlotySymbol = 5(zl)
    val zloty = 3(PLN)
    val dollar = 20.5(USD)

    val total = zlotySymbol + zloty + dollar

    val expected = zlotySymbol.amount + zloty.amount + dollar.amount * conversion((USD, PLN))
    assert(total.currency == PLN)
    assert(total.amount == expected)
  }

  def testSubtraction(): Unit = {
    val dollar = 300.01(USD)
    val euro = 200(EUR)

    val total = dollar - euro

    val expected = dollar.amount - euro.amount * conversion((EUR, USD))
    assert(total.currency == USD)
    assert(total.amount == expected)
  }

  def testMultiplication(): Unit = {
    testMultiplicationZloty()
    testMultiplicationDollar()
  }

  def testMultiplicationZloty(): Unit = {
    val zloty = 30(zl)
    val multiplier = 20

    val total = zloty * multiplier

    val expected = zloty.amount * multiplier
    assert(total.currency == PLN)
    assert(total.amount == expected)
  }

  def testMultiplicationDollar(): Unit = {
    val dollar = 20($)
    val multiplier = 11

    val total = dollar * multiplier

    val expected = dollar.amount * multiplier
    assert(total.currency == USD)
    assert(total.amount == expected)
  }

  def testConversion(): Unit = {
    testDollarToZlotyConversion()
    testDollarToEuroConversion()
  }

  def testDollarToZlotyConversion(): Unit = {
    val dollar = 150.01(USD)

    val conv =  dollar as PLN

    val expected = dollar.amount * conversion((USD, PLN))
    assert(conv.amount == expected)
  }

  def testDollarToEuroConversion(): Unit = {
    val dollar = 120.01(USD)
    val conv =  dollar as E

    val expected = dollar.amount * conversion((USD, EUR))
    assert(conv.amount == expected)
  }

  def testComparison(): Unit = {
    testDollarAndEuroComparison1()
    testDollarAndEuroComparison2()
    testDollarAndEuroComparison3()
  }

  def testDollarAndEuroComparison1(): Unit = {
    val compare = 300.30(USD) > 200(E)

    assert(compare)
  }

  def testDollarAndEuroComparison2(): Unit = {
    val compare = !(300.30(USD) <= 200(E))

    assert(compare)
  }

  def testDollarAndEuroComparison3(): Unit = {
    val compare = 300.30($) < 200(EUR)

    assert(!compare)
  }

  testAddition()
  testSubtraction()
  testMultiplication()
  testConversion()
  testComparison()
}
