package name.abhijitsarkar.scala

import scala.util.{Random, Success, Try}

/**
  * @author Abhijit Sarkar
  */
trait SafeStringUtils {
  // Returns a trimmed version of the string wrapped in an Option,
  // or None if the trimmed string is empty.
  def trimToNone(s: String): Option[String] = {
    Option(s) map (_.trim) filterNot (_.isEmpty)
  }

  /**
    * Q9.2c: Add a method that safely converts a string to an integer, without throwing an error
    * if the string is unparseable. Write and execute tests for valid and invalid input.
    * What are the most appropriate monadic collections to use in this function?
    *
    * Ans: If the input string is unparseable, there is no integer to return,
    * so `Option` seems to be the best choice for the return type.
    */
  def safelyToInt(s: String): Option[Int] =
    Try(trimToNone(s).map(_.toInt)) match {
      case Success(i) => i
      case _ => None
    }

  /**
    * Q9.2e: Add a method that returns a randomly generated string of the given size,
    * limited to only upper and lowercase letters.
    * Write and execute tests that verify the correct contents are return
    * and that invalid input is handled.
    * Are there any appropriate monadic collections to use in this function?
    *
    * Ans: There does not seem to be a benefit from using a monad here. If the input size
    * is less than or equal to zero, the function returns an empty string.
    */
  def randomString(size: Int): String = {
    if (size <= 0) ""
    else Random.alphanumeric.foldLeft("") {
      case (acc, v) if (!v.isDigit) => if (acc.length < size) acc + v else return acc
      case (acc, _) => acc
    }
  }
}

object SafeStringUtils extends SafeStringUtils {

}
