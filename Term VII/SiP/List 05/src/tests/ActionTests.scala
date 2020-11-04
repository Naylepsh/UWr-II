import actions._

package object ActionTests {
//  chaining was already partially tested in plugin tests anyway
  def runTests(): Unit = {
    testActionF()
    testActionG()
  }

  def testActionF(): Unit = {
    val action = actionF
    val text = "abcdef"

    val transformed = action.plugin(text)

    assert(transformed.get == "bcdefa")
  }

  def testActionG(): Unit = {
    val action = actionG
    val text = "x y"

    val transformed = action.plugin(text)

    assert(transformed.get == "x")
  }
}
