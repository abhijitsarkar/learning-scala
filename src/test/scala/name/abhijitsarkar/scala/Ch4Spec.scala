package name.abhijitsarkar.scala

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.{universe => ru}

/**
  * @author Abhijit Sarkar
  */
class Ch4Spec extends FlatSpec with Matchers {
  "Method collect" should "return the values from 5 to 50 by fives" in {
    Ch4.collect(5, 50) should contain inOrderOnly(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  }

  "Method pow" should "raise a to the exponent of b" in {
    val inputAndOutput = Table(
      ("a", "b", "pdt"),
      (1, 0, BigInt(1)),
      (2, 10, BigInt(1024)),
      (5, 3, BigInt(125))
    )

    forAll(inputAndOutput) { (a: Int, b: Int, pdt: BigInt) =>
      Ch4.pow(a, b) should be(pdt)
    }
  }
}
