package name.abhijitsarkar.scala

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.{universe => ru}

/**
  * @author Abhijit Sarkar
  */
class Ch7Spec extends FlatSpec with Matchers {
  "Method fibonacci" should "return the first n Fibonacci numbers" in {
    val methods = Table(
      ("method"),
      ("fibonacci"),
      ("fibonacci2")
    )

    val inputAndOutput = Table(
      ("n", "op"),
      (-1, List()),
      (0, List()),
      (1, List(0)),
      (2, List(0, 1)),
      (8, List(0, 1, 1, 2, 3, 5, 8, 13))
    )

    val im = instanceMirror

    forAll(methods) { m =>
      val fibonacciMethod = ru.typeOf[Ch7.type].decl(ru.TermName(m)).asMethod
      val fibonacci = im.reflectMethod(fibonacciMethod)

      forAll(inputAndOutput) { (n, op) =>
        fibonacci(n) shouldBe op
      }
    }
  }

  private def instanceMirror = {
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val mod = ru.typeOf[Ch7.type].termSymbol.asModule
    val mm = m.reflectModule(mod)
    val obj = mm.instance

    m.reflect(obj)
  }
}
