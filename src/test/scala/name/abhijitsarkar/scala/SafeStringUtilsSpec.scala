package name.abhijitsarkar.scala

import name.abhijitsarkar.scala.SafeStringUtils._
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Abhijit Sarkar
  */
class SafeStringUtilsSpec extends FlatSpec with Matchers {
  "SafeStringUtils" should "trim a string" in {
    val inputAndOutput = Table(
      ("ip", "op"),
      (null, None),
      ("", None),
      ("test", Some("test")),
      ("test  ", Some("test"))
    )

    forAll(inputAndOutput) { (ip, op) =>
      trimToNone(ip) should be(op)
    }
  }

  it should "safely convert a string to an integer" in {
    val inputAndOutput = Table(
      ("ip", "op"),
      (null, None),
      ("", None),
      ("test", None),
      ("1  ", Some(1))
    )

    forAll(inputAndOutput) { (ip, op) =>
      safelyToInt(ip) should be(op)
    }
  }

  it should "generate a random string of the given size" in {
    randomString(-1) shouldBe empty
    randomString(0) shouldBe empty

    val r = randomString(5)
    r should have length 5
    r.foreach {
      isUpperOrLower(_) shouldBe true
    }

    def isUpperOrLower(c: Char) = c.isUpper || c.isLower
  }
}
