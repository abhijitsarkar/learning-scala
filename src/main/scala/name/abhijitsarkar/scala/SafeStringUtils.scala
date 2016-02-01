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

  def safelyToInt(s: String): Option[Int] =
    Try(trimToNone(s).map(_.toInt)) match {
      case Success(i) => i
      case _ => None
    }

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
