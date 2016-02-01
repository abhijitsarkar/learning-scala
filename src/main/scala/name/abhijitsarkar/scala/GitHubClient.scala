package name.abhijitsarkar.scala

import name.abhijitsarkar.scala.GitHubJsonProtocol._
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * @author Abhijit Sarkar
  */

trait GitHubClient {
  def commitLogs(url: String) = {
    val commits = concurrent.Future {
      io.Source.fromURL(url)
    }

    commits.map(_.mkString.parseJson.convertTo[List[CommitLog]])
  }
}

object GitHubStubClient extends GitHubClient {
  def commitLogs(repos: List[Repo]) = {
    val f = super.commitLogs(getClass.getResource("/commits.json").toString)
    Future sequence List(f)
  }
}

object GitHubLiveClient extends GitHubClient {
  def commitLogs(repos: List[Repo]) = {
    Future sequence repos.map { r =>
      val url = s"https://api.github.com/repos/${r.owner}/${r.name}/commits?sha=${r.branch}"

      super.commitLogs(url)
    }
  }
}


