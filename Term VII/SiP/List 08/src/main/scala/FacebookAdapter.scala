import com.restfb.types.User
import com.restfb.{DefaultFacebookClient, Version}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FacebookAdapter {
  private val myAppSecret = "MySuperSecretSecret"

  class MyFacebookClient(currentAccessToken: String)
  extends DefaultFacebookClient(
    currentAccessToken,
    myAppSecret,
    Version.VERSION_5_0) {}

  def getUser(accessToken: String, id: String): Future[User] = Future {
    val client = new MyFacebookClient(accessToken)
    val user = client.fetchObject(id, classOf[User])
    user
  }
}
