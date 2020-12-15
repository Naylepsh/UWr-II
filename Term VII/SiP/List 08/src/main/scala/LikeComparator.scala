import java.util.Calendar
import FileUtils.writeToFile
import com.restfb.types.User
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

class LikeComparator(accessToken: String) {
  private def logResult(logFile: String, user1: User, user2: User): Unit = {
    val current_time = Calendar.getInstance().getTime
    val text = s"$current_time $user1 $user2"
    writeToFile(logFile, List(text))
  }

  private def presentOnScreen(user1: User, user2: User): Unit = {
    val text =
      s"""
         |${user1.getName}, likes: ${user1.getLikes.getTotalCount} vs.
         |${user2.getName}, likes: ${user2.getLikes.getTotalCount}
         |""".stripMargin
    println(text)
  }

  def compareLikes(logFile: String, user1Id: String, user2Id: String): Unit = {
    val user1Future = FacebookAdapter.getUser(accessToken, user1Id)
    val user2Future = FacebookAdapter.getUser(accessToken, user2Id)

    val timeLimit = 30 seconds
    val result = Await.result(user1Future zip user2Future, timeLimit)

    val user1 = result._1
    val user2 = result._2

    logResult(logFile, user1, user2)
    presentOnScreen(user1, user2)
  }
}
