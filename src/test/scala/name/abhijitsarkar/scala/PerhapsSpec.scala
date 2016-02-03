package name.abhijitsarkar.scala

import name.abhijitsarkar.scala.Perhaps._
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Abhijit Sarkar
  */
class PerhapsSpec extends FlatSpec with Matchers {
  "Perhaps" should "should act like a List" in {
    val a: Perhaps[Int] = Perhaps[Int](1).map(_ * 2)
    a should beEqualTo(Perhaps(2))

    val b: Perhaps[Int] = Perhaps(List(1)).flatMap(identity)
    b should beEqualTo(Perhaps(1))

    val c: Perhaps[Int] = Perhaps(List(1, 2)).flatMap(identity).filter(_ % 2 == 0)
    c should beEqualTo(Perhaps(2))
  }

  def beEqualTo(right: Perhaps[Int]) = new PerhapsMatcher(right)

  class PerhapsMatcher[Int](right: Perhaps[Int]) extends Matcher[Perhaps[Int]] {
    override def apply(left: Perhaps[Int]): MatchResult = {
      MatchResult(
        left.a == right.a,
        s"""Perhaps(${left.a}) did not match Perhaps(${right.a})""",
        s"""Perhaps(${left.a}) matched Perhaps(${right.a})"""
      )
    }
  }

}
