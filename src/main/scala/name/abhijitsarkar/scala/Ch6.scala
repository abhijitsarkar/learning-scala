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

  /**
    * Q6.3: Write a function, `first[A](items: List[A], count: Int): List[A]`, that returns
    * the first x number of items in a given list.
    * For example, `first(List('a','t','o'), 2)` should return `List('a','t')`.
    * You could make this a one-liner by invoking one of the built-in list operations
    * that already performs this task, or (preferably) implement your own solution.
    * Can you do so with a `for` loop? With `foldLeft`? With a recursive function that only accesses head and tail?
    */
  def first[A](items: List[A], count: Int): List[A] = {
    val seq = for (x <- 0 until math.min(count, items.size); y = items(x)) yield y

    seq.toList
  }

  def first2[A](items: List[A], count: Int): List[A] = {
    items.foldLeft(Option((0, List[A]()))) {
      (acc, a) => acc.map(x => if (x._1 < count) (x._1 + 1, x._2 :+ a) else return x._2)
    }.get._2
  }

  def first3[A](items: List[A], count: Int) = {
    @annotation.tailrec
    def go(tail: List[A], x: Int, acc: List[A]): List[A] =
      tail match {
        case _ if x >= tail.size => tail
        case _ if x <= 0 => acc
        case h :: t => go(t, x - 1, acc :+ h)
        case Nil => Nil
      }

    go(items, count, Nil)
  }

  /**
    * Q6.4: Write a function that takes a list of strings and returns the longest string in the list.
    * Can you avoid using mutable variables here?
    * This is an excellent candidate for the list-folding operations (Table 6-5) we studied.
    * Can you implement this with both `fold` and `reduce`?
    * Would your function be more useful if it took a function parameter
    * that compared two strings and returned the preferred one?
    * How about if this function was applicable to generic lists, i.e., lists of any type?
    *
    * Ans: `fold` cannot be applied to a generic list without a "zero" element.
    * The solution using `reduce` works for a generic list but it returns `null` for empty lists.
    */
  def longest(items: List[String], f: (String, String) => String) =
    if (items.isEmpty) ""
    else items.fold("") { (acc, a) => f(acc, a) }

  def longest2[A](items: List[A], f: (A, A) => A) =
    if (items.isEmpty) null
    else items.reduce(f)

  /**
    * Q6.5: Write a function that reverses a list. Can you write this as a recursive function?
    */
  def reverse[A](items: List[A]) = {
    @annotation.tailrec
    def go(l: List[A], acc: List[A]): List[A] =
      l match {
        case Nil => acc
        case h :: t => go(t, h :: acc)
      }

    go(items, Nil)
  }

  /**
    * Q6.6: Write a function that takes a `List[String]` and returns a `(List[String],List[String])`,
    * a tuple of string lists. The first list should be items in the original list
    * that are palindromes (written the same forward and backward, like "racecar").
    * The second list in the tuple should be all of the remaining items from the original list.
    * You can implement this easily with partition, but are there other operations you could use instead?
    *
    * Ans: I could use the `fold` and `reduce` methods and also the `toMap` method with a `Boolean` `true`
    * key for the palindromes and `false` for the rest.
    * There might be some other methods too if I looked harder.
    */
  def partition(l: List[String]) = {
    @annotation.tailrec
    def go(items: List[String], palindromes: List[String], rest: List[String]): (List[String], List[String]) =
      items match {
        case Nil => (palindromes, rest)
        case h :: t if h == h.reverse => go(t, h :: palindromes, rest)
        case h :: t => go(t, palindromes, h :: rest)
      }

    go(l, Nil, Nil)
  }
}
