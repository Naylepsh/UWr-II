import ExceptionHandler.unSafe

object ExceptionHandlerTest {
  class MyException extends Exception

  def run(): Unit = {
    try {
      unSafe(new MyException()) {
        generateError()
      }
    } catch {
      case _: MyException => ()
      case err: Throwable => println(s"invalid error, got ${err.getMessage}")
    }
  }

  def generateError(): Int = {
    1 / 0
  }
}
