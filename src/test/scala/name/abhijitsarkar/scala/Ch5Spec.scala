package name.abhijitsarkar.scala

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.{universe => ru}

/**
  * @author Abhijit Sarkar
  */
class Ch5Spec extends FlatSpec with Matchers {
  "Method typesafe" should "print the number, or any of the words 'type', 'safe' or 'typesafe' as applicable" in {
    val methods = Table(
      ("method"),
      ("typesafe"),
      ("typesafe2")
    )

    val im = instanceMirror

    forAll(methods) { m =>
      val typesafeMethod = ru.typeOf[Ch5.type].decl(ru.TermName(m)).asMethod
      val typesafe = im.reflectMethod(typesafeMethod)

      typesafe()
    }
  }

  private def instanceMirror = {
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val mod = ru.typeOf[Ch5.type].termSymbol.asModule
    val mm = m.reflectModule(mod)
    val obj = mm.instance

    m.reflect(obj)
  }
}
