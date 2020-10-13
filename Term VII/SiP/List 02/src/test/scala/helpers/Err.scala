package test.scala.helpers

object Err {
  // a tiny hack for assertThrow without using ScalaTest
  def assertThrow[T](f: () => Unit, message: String = "function did not throw an error"): Unit = {
    try {
      f()
      assert(assertion = false, message)
    } catch {
      case _: T => ()
      case _: Throwable => assert(assertion = false, message)
    }
  }

  def createErrorMessage(operation: String, args: List[Any], result: Any): String = {
    operation + " failed on args " + args.mkString(", ") + ", got: " + result.toString
  }
}
