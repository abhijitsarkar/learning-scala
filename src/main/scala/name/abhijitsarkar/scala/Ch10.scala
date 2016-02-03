package name.abhijitsarkar.scala

/**
  * @author Abhijit Sarkar
  */
object Ch10 {
  /**
    * Q10.1: How would you extend a function? What are some of the applications for a class or trait
    * that extends `Function1[A,B]`? If you are writing such a class or trait,
    * would you extend `Function1[A,B]` or choose to extend `A => B` ?
    *
    * Ans: There can be numerous applications for extending `Function1[A,B]`. Imagine a class that models
    * complex numbers. The additive inverse of a complex number is one that produces a value of
    * zero when it is added to the original complex number. We could use a `Function1[Complex, Complex]` for this.
    * As another example, a string utility object could contains several `Function1[String, A]` methods
    * that safely trim a string or safely convert to the type `A`.
    *
    * Extending `A => B` is adequate when there are no additional parameters needed for the function to operate,
    * and the arguments to the `apply` method are sufficient. Otherwise, we need to extend `Function1` with a
    * constructor argument that will later be used by the `apply` method. As an example, consider a filter
    * function `A => Boolean`. In order to make a determination in the `apply` method,
    * we may need more information passed to the constructor.
    */

  /**
    * Q10.2: How would you write a function type for a function that has two parameter lists,
    * each with a single integer, and returns a single integer?
    * If you wrote it as a FunctionX class, what would the exact class and type parameters contain?
    *
    * Ans:
    * {{{
    * def f(a: Int)(b: Int): Int = ???
    * Function2[Int, Int, Int]
    * }}}
    */

  /**
    * Q10.3 :A popular use for implicit parameters is for a default setting that works most of the time
    * but may be overridden in special cases. Assume you are writing a sorting function
    * that takes lines of text, and the lines may start with a right-aligned number.
    * If you want to sort using the numbers, which may be prefixed by spaces,
    * how would you encode this ability in an implicit parameter?
    * How would you allow users to override this behavior and ignore the numbers for sorting?
    *
    * Ans: Check out the test cases for usage.
    * Good reads:
    * [[http://docs.scala-lang.org/tutorials/FAQ/finding-implicits.html Where does Scala look for implicits?]]
    * [[http://stackoverflow.com/questions/19345030/easy-idiomatic-way-to-define-ordering-for-a-simple-case-class easy idiomatic way to define Ordering for a simple case class]]
    */
  def sortLines(l: List[String])(implicit o: Ordering[String]) = l.sorted

  /**
    * Q10.6: How would you add a `sum` method on all tuples, which returns the sum of all numeric values in a tuple?
    * For example, `('a', "hi", 2.5, 1, true).sum` should return 3.5
    *
    * Ans: Check out http://stackoverflow.com/questions/35148463/scala-implicit-conversion-of-any-to-numeric/
    */
  implicit class PimpedProduct(val p: Product) {
    def sum = p.productIterator.collect {
      case x: java.lang.Number => x.doubleValue
    }.sum
  }

}
