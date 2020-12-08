object Main extends App {
  val accessToken = "SomeAccessTokenHere"
  val user1Id = "1"
  val user2Id = "2"
  val logFile = "./likes.logs.txt"

  val likeComparator = new LikeComparator(accessToken)

  likeComparator.compareLikes(logFile, user1Id, user2Id)
}
