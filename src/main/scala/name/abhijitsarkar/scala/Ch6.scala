package name.abhijitsarkar.scala

/**
  * @author Abhijit Sarkar
  */
object Ch6 {
  /**
    * Q6.1: Create a list of the first 20 odd Long numbers.
    * Can you create this with a for-loop, with the filter operation, and with the map operation?
    * What’s the most efficient and expressive way to write this?
    */
  def first20 = {
    /* Note that y uses a '=', not a '<-'. For the later, it acts as a nested for loop,
     * with each value of x iterated over all values of y.
     */
    for (x <- 1l to 20; y = x * 2 - 1) yield y // equivalent: (1l to 20).map(_ * 2 - 1).map(identity)

    for (x <- 1l to 40 if x % 2 > 0) yield x // equivalent: (1l to 40).withFilter(_ % 2 > 0).map(identity)
  }

  /**
    * Q6.2: Write a function titled “factors” that takes a number and returns a list of its factors,
    * other than 1 and the number itself. For example, factors(15) should return List(3, 5).
    * Then write a new function that applies “factors” to a list of numbers.
    * Try using the list of Long numbers you generated in exercise 1.
    * For example, executing this function with List(9, 11, 13, 15) should return List(3, 3, 5),
    * because the factor of 9 is 3 while the factors of 15 are 3 again and 5.
    * Is this a good place to use map and flatten? Or would a for-loop be a better fit?
    */
  def factors(a: Long) =
    for (x <- 2l until a if a % x == 0) yield x

  def flatFactors(l: List[Long]) =
    l.flatMap(factors) // equivalent: for (x <- l; y <- factors(x)) yield y
  /* Note that for (x <- l; y = factors(x)) yield y produces
   *  List(Vector(3), Vector(), Vector(), Vector(3, 5))
   */
}
