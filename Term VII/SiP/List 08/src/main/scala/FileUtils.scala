import java.io.{File, FileWriter}

object FileUtils {
  def writeToFile(filename: String, lines: Seq[String]): Unit = {
    val file = new File(filename)
    val writer = new FileWriter(file)
    try {
      for (line <- lines) {
        writer.write(line)
      }
    } catch {
      case e: Throwable => throw e
    } finally {
      writer.close()
    }
  }
}
