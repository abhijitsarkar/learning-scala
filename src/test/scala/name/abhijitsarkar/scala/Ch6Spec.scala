package name.abhijitsarkar.scala

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.{universe => ru}

/**
  * @author Abhijit Sarkar
  */
class Ch6Spec extends FlatSpec with Matchers {
  "Method first" should "return the first n number of items" in {
    val methods = Table(
      ("method"),
      ("first"),
      ("first2"),
      ("first3")
    )

    val inputAndOutput = Table(
      ("ip", "n", "op"),
      (List(1, 2, 3), -1, Nil),
      (List(1, 2, 3), 0, Nil),
      (List(1, 2, 3), 1, List(1)),
      (List(1, 2, 3), 2, List(1, 2)),
      (List(1, 2, 3), 3, List(1, 2, 3)),
      (List(1, 2, 3), 4, List(1, 2, 3))
    )

    val im = instanceMirror

    forAll(methods) { m =>
      val firstMethod = ru.typeOf[Ch6.type].decl(ru.TermName(m)).asMethod
      val first = im.reflectMethod(firstMethod)

      forAll(inputAndOutput) { (ip, n, op) =>
        first(ip, n) shouldBe op
      }
    }
  }

  "Method reverse" should "reverse a string" in {
    val inputAndOutput = Table(
      ("ip", "op"),
      ("", ""),
      ("abc", "cba")
    )

    forAll(inputAndOutput) { (ip, op) =>
      Ch6.reverse[Char](ip.toList).mkString("") shouldBe op
    }
  }

  "Method partition" should "partition palindromes and non-palindromes" in {
    val inputAndOutput = Table(
      ("ip", "op"),
      (Nil, (Nil, Nil)),
      (List("racecar", "whatever"), (List("racecar"), List("whatever")))
    )

    forAll(inputAndOutput) { (ip, op) =>
      Ch6.partition(ip) shouldBe op
    }
  }

  private def instanceMirror = {
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val mod = ru.typeOf[Ch6.type].termSymbol.asModule
    val mm = m.reflectModule(mod)
    val obj = mm.instance

    m.reflect(obj)
  }
}
