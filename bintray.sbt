import adept.bintray._
import adept.bintray.BintrayKeys._

bintrayInfo in GlobalScope := {
  //must be filled out correctly:
  val apiKey = "SECRET"
  val subject = "SECRET"
  val repoName = "SECRET"
  BintrayInfo(subject, apiKey, repoName)
}
