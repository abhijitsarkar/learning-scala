package name.abhijitsarkar.scala

import scala.collection.mutable.Buffer

/**
  * @author Abhijit Sarkar
  */
object Ch7 {
  /**
    * Q7.1a: Write a function that returns a list of the first x elements in the Fibonacci series.
    * Can you write this with a Buffer? Would a Builder be appropriate here?
    *
    * Ans: A Builder is not appropriate here because it does not have a `take` method that I used
    * for an early exit.
    */
  def fibonacci(x: Int) = {
    @annotation.tailrec
    def loop(n: Int, prev: Long, cur: Long, acc: Buffer[Long]): Buffer[Long] =
      if (n <= 2) acc
      else loop(n - 1, cur, prev + cur, acc += (prev + cur))

    val acc = Buffer[Long](0, 1)

    val ret = if (x <= 2) acc.take(math.max(x, 0))
    else loop(x, 0, 1, acc)

    ret.toList
  }

  /**
    * Q7.1c: The `Stream` collection is a great solution for creating a Fibonacci series.
    * Create a stream that will generate a Fibonacci series.
    * Use it to print out the first 100 elements in the series,
    * in a formatted report of 10 comma-delimited elements per line.
    */
  def fibonacci2(x: Int) = {
    @annotation.tailrec
    def loop(n: Int, prev: Long, cur: Long, acc: Stream[Long]): Stream[Long] =
      if (n <= 2) acc
      else loop(n - 1, cur, prev + cur, (prev + cur) #:: acc)

    val acc = Stream[Long](0, 1)

    val ret = if (x <= 2) acc.take(math.max(x, 0))
    else loop(x, 0, 1, Stream(1, 0)).reverse

    ret.toList
  }
}
