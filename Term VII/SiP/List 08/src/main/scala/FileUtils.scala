import java.io.{File, FileWriter}

object FileUtils {
  def writeToFile(filename: String, lines: Seq[String]): Unit = {
    val file = new File(filename)
    val append = true
    val writer = new FileWriter(file, append)
    try {
      for (line <- lines) {
        writer.write(line + "\n")
      }
    } catch {
      case e: Throwable => throw e
    } finally {
      writer.close()
    }
  }
}
