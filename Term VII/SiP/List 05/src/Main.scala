import foo._
object Main extends App {
  PluginTests.runTests()
  ActionTests.runTests()

//  val pl = new Plugin with Reverting with Repeat
//  val text = "text"
//  print(pl.plugin(text))
}
