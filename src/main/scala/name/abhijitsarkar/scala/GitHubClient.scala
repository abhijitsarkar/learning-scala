package name.abhijitsarkar.scala

import name.abhijitsarkar.scala.GitHubJsonProtocol._
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * @author Abhijit Sarkar
  */
/**
  * Q7.6: Write a function that reports recent GitHub commits for a project.
  * GitHub provides an RSS feed of recent commits for a given user,
  * repository, and branch, containing XML that you can parse out with regular expressions.
  * Your function should take the user, repository, and branch, read and parse the RSS feed,
  * and then print out the commit information.
  * This should include the date, title, and author of each commit.
  *
  * Ans: Instead of parsing XML using regex, which is a bad idea, I used the `spray-json` library
  * and GitHub REST API to get and parse JSON.
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


