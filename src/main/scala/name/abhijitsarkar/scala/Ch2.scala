package name.abhijitsarkar.scala

/**
  * @author Abhijit Sarkar
  */
object Ch2 {
  /**
    * Q2.6: Using the input string "Frank,123 Main,925-555-1943,95122" and regular expression matching,
    * retrieve the telephone number. Can you convert each part of the telephone number to its own integer value?
    * How would you store this in a tuple?
    */
  def extractPhNum(addr: String): (Int, Int, Int) = {
    val Pattern = """(?:.*?),([0-9]{3})[-]([0-9]{3})[-]([0-9]{4}),(?:[0-9]{5})""".r

    addr match {
      case Pattern(areaCode, prefix, lineNum) => (areaCode.toInt, prefix.toInt, lineNum.toInt)
      case _ => throw new MatchError(s"No match for address: $addr.")
    }
  }

  def extractPhNum2(addr: String): (Int, Int, Int) = {
    val Address(areaCode, prefix, lineNum) = addr

    (areaCode, prefix, lineNum)
  }
}

/* Using Name Based Extractors and Value class.
 * http://hseeberger.github.io/blog/2013/10/04/name-based-extractors-in-scala-2-dot-11/
 */
class Address(val phNum: (String, String, String)) extends AnyVal {
  def isEmpty = phNum == null

  def get: (Int, Int, Int) = {
    (phNum._1.toInt, phNum._2.toInt, phNum._3.toInt)
  }
}

object Address {
  val Pattern = """(?:.*?),([0-9]{3})[-]([0-9]{3})[-]([0-9]{4}),(?:[0-9]{5})""".r

  def unapply(addr: String): Address = {
    addr match {
      case Pattern(areaCode, prefix, lineNum) => new Address(areaCode, prefix, lineNum)
      case _ => new Address(null)
    }
  }
}
