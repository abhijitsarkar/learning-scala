package name.abhijitsarkar.scala

import name.abhijitsarkar.scala.GitHubJsonProtocol.Repo
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.reflect.runtime.{universe => ru}

/**
  * @author Abhijit Sarkar
  */
class GitHubStubClientSpec extends FlatSpec with Matchers {
  "Stub client" should "return the list of commits" in {
    val repo = Repo("learning-scala", "abhijitsarkar")

    val f = GitHubStubClient.commitLogs(List(repo))
    val l = concurrent.Await.result(f, Duration(3, SECONDS)).head

    l.foreach { log =>
      println(f"${repo.name} | ${log.sha}%s | ${log.commit.author.name}%s | ${log.commit.author.date}%s | ${log.commit.message}%s")
    }
  }

  "Live client" should "return the list of commits" in {
    val repo = Repo("learning-scala", "abhijitsarkar")

    val f = GitHubLiveClient.commitLogs(List(repo))
    val l = concurrent.Await.result(f, Duration(3, SECONDS)).flatten

    l.foreach { log =>
      println(f"${repo.name} | ${log.sha}%s | ${log.commit.author.name}%s | ${log.commit.author.date}%s | ${log.commit.message}%s")
    }
  }
}
