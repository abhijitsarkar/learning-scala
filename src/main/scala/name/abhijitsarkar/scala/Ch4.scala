package name.abhijitsarkar.scala

import scala.collection.immutable.List

/**
  * @author Abhijit Sarkar
  */
object Ch4 {
  def collect(begin: Int, end: Int, step: Int = 5) = {
    @annotation.tailrec
    def loop(acc: List[Int], a: Int): List[Int] =
      if (a < begin) acc
      else loop(a :: acc, a - step)

    loop(List(), end)
  }

  def pow(a: Int, b: Int) = {
    @annotation.tailrec
    def loop(pdt: BigInt, x: Int): BigInt =
      if (x <= 0) pdt
      else loop(pdt * a, x - 1)

    loop(BigInt(1), b)
  }
}
