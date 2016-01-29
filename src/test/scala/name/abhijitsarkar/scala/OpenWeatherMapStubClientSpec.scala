package name.abhijitsarkar.scala

import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.{universe => ru}

/**
  * @author Abhijit Sarkar
  */
class OpenWeatherMapStubClientSpec extends FlatSpec with Matchers {
  "Method city" should "return the city name and country code" in {
    OpenWeatherMapStubClient.cities() should be("Whitby, GB")
  }

  "Method numForecasts" should "return the number of forecast segments" in {
    OpenWeatherMapStubClient.numForecasts should be(37)
  }

  "Method descriptions" should "return the descriptions" in {
    val desc = OpenWeatherMapStubClient.descriptions().filter(x => x._1.contains("scattered clouds")).take(1)

    val idx = desc.head._2

    idx should be > 0
    idx should be <= 12
  }

  "Method symbolMap" should "return a map from the symbol number to the description" in {
    OpenWeatherMapStubClient.symbolMap() should contain value "scattered clouds"
  }

  "Method temp" should "return temperature" in {
    OpenWeatherMapStubClient.temp().size should be (24)
  }

  "Method avgTemp" should "return average temperature" in {
    println(OpenWeatherMapStubClient.avgTemp())
  }
}
