package name.abhijitsarkar.scala

/**
  * @author Abhijit Sarkar
  */

/**
  * Q7: We’ll be reading and processing a forecast from the OpenWeatherMap API.
  */
object OpenWeatherMapStubClient {
  final val NameAndCountry = """<name>(.*?)</name>\|<country>(.*?)</country>""".r
  final val Description = """<symbol number="(?:.*?)" name="(.*?)" var="(?:.*?)" />""".r
  final val NumberAndDescription = """<symbol number="(.*?)" name="(.*?)" var="(?:.*?)" />""".r
  final val Temperature = """<temperature unit="celsius" value="(.*?)" min="(.*?)" max="(.*?)" />""".r

  private def readFromFile =
    io.Source.fromURL(getClass.getResource("/forecast.xml")).getLines.toList

  /**
    *
    * Q7.b: The forecast’s city’s name is there in the first 10 lines. Grab it from the correct line and
    * print out its XML element. Then extract the city name and country code from their XML elements
    * and print them out together (e.g., "Paris, FR").
    * This is a good place to use regular expressions to extract the text from XML tags.
    */
  def cities(n: Int = 10) = {
    val tmp = readFromFile
      .take(n)
      .filter(l => l.contains("<name>") || l.contains("<country>"))
      .map(_.trim)
      .mkString("|")

    tmp match {
      case NameAndCountry(name, country) => s"$name, $country"
      case _ => ""
    }
  }

  /**
    * Q7.c: How many forecast segments are there?
    * What is the shortest expression you can write to count the segments?
    */
  def numForecasts = readFromFile.count(_.contains("time from"))

  def descriptions(n: Int = 12) = readFromFile
    .filter(_.contains("symbol number"))
    .take(n)
    .map {
      _.trim match {
        case Description(desc) => s"$desc"
        case _ => ""
      }
    }
    .zipWithIndex
    .map(x => (x._1, x._2 + 1))

  /**
    * Q7.d: The "symbol" XML element in each forecast segment includes a description of the weather forecast.
    * Extract this element in the same way you extracted the city name and country code.
    * Try iterating through the forecasts, printing out the description.
    * Then create an informal weather report by printing out the weather descriptions over the next 12 hours
    * (not including the XML elements).
    *
    * Q7.f: These descriptions may be useful later. Included in the "symbol" XML element
    * is an attribute containing the symbol number.
    * Create a Map from the symbol number to the description.
    * Verify this is accurate by manually accessing symbol values from the forecast
    * and checking that the description matches the XML document.
    */
  def symbolMap(n: Int = 12): Map[Int, String] = readFromFile
    .filter(_.contains("symbol number"))
    .take(n)
    .map {
      _.trim match {
        case NumberAndDescription(num, desc) => (num.toInt, desc)
        case _ => (0, "")
      }
      /* TL; DR. breakout detail. http://stackoverflow.com/questions/1715681/scala-2-8-breakout */
    }(collection.breakOut)

  /**
    * Q7.g: What are the high and low temperatures over the next 24 hours?
    *
    * Q7.h: What is the average temperature in this weather forecast?
    * You can use the "value" attribute in the temperature element to calculate this value.
    */
  def temp(n: Int = 24) = readFromFile
    .filter(_.contains("temperature unit"))
    .take(n)
    .map {
      _.trim match {
        case Temperature(avg, hi, lo) => (avg.toFloat, hi.toFloat, lo.toFloat)
        case _ => (0.0f, 0.0f, 0.0f)
      }
    }

  /**
    * Q7.h: What is the average temperature in this weather forecast?
    * You can use the "value" attribute in the temperature element to calculate this value.
    */
  def avgTemp(n: Int = 24) = {
    val t = temp().map(_._2).zipWithIndex.reduce { (x, y) => (x._1 + y._1, y._2 + 1) }

    t._1 / t._2
  }
}
