import Utils.{compose, curry, isAscSorted, isDescSorted, isSorted, length, repeated, sum, uncurry}

object UtilsTest {
  def run(): Unit = {
    isSortedTest()
    isAscSortedTest()
    isDescSortedTest()
    sumTest()
    lengthTest()
    composeTest()
    repeatedTest()
    curryTest()
    uncarryTest()
  }

  def isSortedTest() : Unit = {
    isSortedOnEmptyList()
    isSortedOnOneElemList()
    isSortedOnSortedList()
    isSortedOnUnsortedList()
  }

  def isAscSortedTest(): Unit = {
    isAscSortedOnAscList()
    isAscSortedOnDescList()
    isAscSortedOnChaoticList()
  }

  def isDescSortedTest(): Unit = {
    isDescSortedOnAscList()
    isDescSortedOnDescList()
    isDescSortedOnChaoticList()
  }

  def sumTest(): Unit = {
    sumOnEmptyList()
    sumOnOneElemList()
    sumOnMultiElemsList()
  }

  def lengthTest(): Unit = {
    lengthOnEmptyList()
    lengthOnOneElemList()
    lengthOnMultiElemsList()
  }

  def composeTest(): Unit = {
    composeOnIdentity()
    composeOnPlusOne()
  }

  def repeatedTest(): Unit = {
    repeatedOnIdentity()
    repeatedOnPlusOne()
    repeatedOnNonPositiveN()
  }

  def curryTest(): Unit = {
    val a = 1
    val b = 2

    val res = curry(add)(a)(b)
    val expected = add(a, b)

    assert(res == expected, s"curry on add and $a, $b failed, expected $expected, got $res")
  }

  def uncarryTest(): Unit = {
    val a = 1
    val b = 2
    val curried = curry(add)

    val res = uncurry(curried)(a, b)
    val expected = add(a, b)

    assert(res == expected, s"uncurry on add and $a, $b failed, expected $expected, got $res")
  }

  def isSortedOnSortedList(): Unit = {
    val xs = List(1,2,3)

    val res = isSorted(xs, (x, y) => x < y)

    assert(res, s"isSorted on $xs and (x, y) => x < y failed")
  }

  def isSortedOnUnsortedList(): Unit = {
    val xs = List(3,1,2)

    val res = isSorted(xs, (x, y) => x < y)

    assert(res, s"isSorted on $xs and (x, y) => x < y failed")
  }

  def isSortedOnEmptyList(): Unit = {
    val xs = List()

    val res = isSorted(xs, (x, y) => x < y)

    assert(res, s"isSorted on $xs and (x, y) => x < y failed")
  }

  def isSortedOnOneElemList(): Unit = {
    val xs = List(1)

    val res = isSorted(xs, (x, y) => x < y)

    assert(res, s"isSorted on $xs and (x, y) => x < y failed")
  }

  def isAscSortedOnAscList(): Unit = {
    val xs = List(1, 2, 3)

    val res = isAscSorted(xs)

    assert(res, s"isAscSorted on $xs failed")
  }

  def isAscSortedOnDescList(): Unit = {
    val xs = List(3, 2, 1)

    val res = isAscSorted(xs)

    assert(res, s"isAscSorted on $xs failed")
  }

  def isAscSortedOnChaoticList(): Unit = {
    val xs = List(3, 1, 2)

    val res = isAscSorted(xs)

    assert(res, s"isAscSorted on $xs failed")
  }

  def isDescSortedOnAscList(): Unit = {
    val xs = List(1, 2, 3)

    val res = isDescSorted(xs)

    assert(res, s"isAscSorted on $xs failed")
  }

  def isDescSortedOnDescList(): Unit = {
    val xs = List(3, 2, 1)

    val res = isDescSorted(xs)

    assert(res, s"isAscSorted on $xs failed")
  }

  def isDescSortedOnChaoticList(): Unit = {
    val xs = List(3, 1, 2)

    val res = isDescSorted(xs)

    assert(res, s"isAscSorted on $xs failed")
  }

  def sumOnEmptyList(): Unit = {
    val xs = List()

    val res = sum(xs)
    val expected = 0

    assert(res == expected, s"sum on $xs failed - expected $expected, got $res")
  }

  def sumOnOneElemList(): Unit = {
    val xs = List(1)

    val res = sum(xs)
    val expected = xs.head

    assert(res == expected, s"sum on $xs failed - expected $expected, got $res")
  }

  def sumOnMultiElemsList(): Unit = {
    val xs = List(1, 2, 3)

    val res = sum(xs)
    val expected = 6

    assert(res == expected, s"sum on $xs failed - expected $expected, got $res")
  }

  def lengthOnEmptyList(): Unit = {
    val xs = List()

    val res = length(xs)
    val expected = 0

    assert(res == expected, s"length on $xs failed - expected $expected, got $res")
  }

  def lengthOnOneElemList(): Unit = {
    val xs = List(1)

    val res = length(xs)
    val expected = 1

    assert(res == expected, s"length on $xs failed - expected $expected, got $res")
  }

  def lengthOnMultiElemsList(): Unit = {
    val xs = List(1, 2, 3)

    val res = length(xs)
    val expected = 3

    assert(res == expected, s"length on $xs failed - expected $expected, got $res")
  }

  def composeOnIdentity(): Unit = {
    val x = 1

    val res = compose(identity, identity)(x)

    assert(res == x, s"compose on x => x with x = $x failed")
  }

  def composeOnPlusOne(): Unit = {
    val x = 2

    val res = compose(plusOne, plusOne)(x)
    val expected = x + 2

    assert(res == expected, s"compose on x => x + 1 with x = $x failed, expected $expected, got $res")
  }

  def repeatedOnIdentity(): Unit = {
    val x = 1
    val n = 10

    val res = repeated(identity, n)(x)
    val expected = x

    assert(res == expected, s"repeated on x => x with x = $x failed, expected $expected, got $res")
  }

  def repeatedOnPlusOne(): Unit = {
    val x = 1
    val n = 10

    val res = repeated(plusOne, n)(x)
    val expected = x + n

    assert(res == expected, s"repeated on x => x + 1 with x = $x failed, expected $expected, got $res")
  }

  def repeatedOnNonPositiveN(): Unit = {
    val x = 1
    val n = 0

    try {
      repeated(plusOne, n)(x)
      throw new Exception("repeated on non positive n did not throw an error")
    } catch {
      case _: IllegalArgumentException => ()
      case err: Throwable => throw err
    }
  }

  private def identity(x: Any): Any = {
    x
  }

  private def plusOne(x: Int): Int = {
    x + 1
  }

  private def add(a: Int, b: Int) = a + b
}
