package name.abhijitsarkar.scala

/**
  * @author Abhijit Sarkar
  */
object Ch7 {
  def fibonacci(x: Int) = {
    @annotation.tailrec
    def loop(n: Int, prev: Long, cur: Long, acc: Seq[Long]): Seq[Long] =
      if (n <= 2) acc
      else loop(n - 1, cur, prev + cur, acc :+ (prev + cur))

    val acc = Seq[Long](0, 1)

    val ret = if (x <= 2) acc.take(math.max(x, 0))
    else loop(x, 0, 1, acc)

    ret.toList
  }

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
