object ExceptionHandler {
  def unSafe[T](ex: Exception)(block: => T): T = {
    try {
      block
    } catch {
      case error: Throwable =>
        println(s"logging error: ${error.getMessage}")
        throw ex
    }
  }
}
