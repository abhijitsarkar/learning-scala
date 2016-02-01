package name.abhijitsarkar.scala

import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.{universe => ru}

/**
  * @author Abhijit Sarkar
  */
class DirSpec extends FlatSpec with Matchers {
  "list method" should "return the list of all class files" in {
    val root = getClass.getResource(".").getFile
    val l = new Dir(root, _.endsWith(".class")).list

    println(l)
  }
}
