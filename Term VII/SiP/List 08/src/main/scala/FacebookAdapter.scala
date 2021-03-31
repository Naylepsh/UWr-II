import com.restfb.types.User
import com.restfb.{DefaultFacebookClient, Version}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FacebookAdapter {
  //SIP For safety could be like: 
  //private val myAppSecret = sys.env.get("FB_APP_SECRET") match {
  //  case Some(value) => value
  //  case None => throw new Exception("FB_APP_SECRET is not defined")
  //To separate code from sensitive data
  private val myAppSecret = "MySuperSecretSecret"

  private class MyFacebookClient(currentAccessToken: String)
  extends DefaultFacebookClient(
    currentAccessToken,
    myAppSecret,
    Version.VERSION_5_0) {}

  //SIP Great that FacebookAdapter has only one method - getUser & no logic
  //Adapters should only do objects mapping
  def getUser(accessToken: String, id: String): Future[User] = Future {
    val client = new MyFacebookClient(accessToken)
    val user = client.fetchObject(id, classOf[User])
    user
  }
}
