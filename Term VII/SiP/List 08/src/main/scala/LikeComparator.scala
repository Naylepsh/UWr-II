import java.util.Calendar
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import FileUtils.writeToFile
import com.restfb.types.User

class LikeComparator(accessToken: String) {
  private def logResult(logFile: String, user1: User, user2: User)= Future {
    val current_time = Calendar.getInstance().getTime
    val line = s"$current_time $user1 $user2"
    writeToFile(logFile, List(line))
  }

  private def presentOnScreen(user1: User, user2: User)= Future {
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
    val (user1, user2) = Await.result(user1Future zip user2Future, 30 seconds)

    val logFuture = logResult(logFile, user1, user2)
    val presentFuture = presentOnScreen(user1, user2)
    Await.result(logFuture zip presentFuture, 30 seconds)
  }
}
