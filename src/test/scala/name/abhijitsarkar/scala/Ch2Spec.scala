package name.abhijitsarkar.scala

import java.lang.reflect.InvocationTargetException

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.{universe => ru}

/**
  * @author Abhijit Sarkar
  */
class Ch2Spec extends FlatSpec with Matchers {
  "Method extractPhNum" should "extract a phone number from address" in {
    val methods = Table(
      ("method"),
      ("extractPhNum"),
      ("extractPhNum2")
    )

    val im = instanceMirror

    forAll(methods) { m =>
      val extractPhNumMethod = ru.typeOf[Ch2.type].decl(ru.TermName(m)).asMethod
      val extractPhNum = im.reflectMethod(extractPhNumMethod)

      extractPhNum("Frank,123 Main,925-555-1943,95122") should be((925, 555, 1943))
    }
  }

  "Method extractPhNum" should "throw an exception for unexpected address format" in {
    val methods = Table(
      ("method"),
      ("extractPhNum"),
      ("extractPhNum2")
    )

    val im = instanceMirror

    forAll(methods) { m =>
      val extractPhNumMethod = ru.typeOf[Ch2.type].decl(ru.TermName(m)).asMethod
      val extractPhNum = im.reflectMethod(extractPhNumMethod)

      intercept[InvocationTargetException] {
        extractPhNum("junk")
      }
    }
  }

  private def instanceMirror = {
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val mod = ru.typeOf[Ch2.type].termSymbol.asModule
    val mm = m.reflectModule(mod)
    val obj = mm.instance

    m.reflect(obj)
  }
}
