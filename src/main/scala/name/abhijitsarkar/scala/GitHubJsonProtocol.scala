package name.abhijitsarkar.scala

import java.time.Instant
import java.time.format.DateTimeFormatter._

import spray.json._

/**
  * @author Abhijit Sarkar
  */
object GitHubJsonProtocol extends DefaultJsonProtocol {
  implicit object InstantJsonFormat extends JsonFormat[Instant] {
    def write(i: Instant) =
      JsString(ISO_INSTANT.format(i))

    def read(value: JsValue) = value match {
      case JsString(str) => Instant.parse(str)
      case _ => deserializationError(s"Failed to deserialize $value.")
    }
  }

  case class Author(name: String, date: Instant)

  case class Commit(author: Author, message: String)

  case class CommitLog(sha: String, commit: Commit)

  case class Repo(name: String, owner: String, branch: String = "master")

  implicit val authorFormat = jsonFormat2(Author)
  implicit val commitFormat = jsonFormat2(Commit)
  implicit val commitLogFormat = jsonFormat2(CommitLog)
  implicit val repoFormat = jsonFormat3(Repo)
}
