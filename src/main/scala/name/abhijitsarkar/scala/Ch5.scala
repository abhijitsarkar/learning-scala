package name.abhijitsarkar.scala

/**
  * @author Abhijit Sarkar
  */
object Ch5 {
  /** Q5.1: Write a function literal that takes two integers and returns the higher number.
    * Then write a higher-order function that takes a 3-sized tuple of integers plus this function literal,
    * and uses it to return the maximum value in the tuple.
    */
  def max(ints: Tuple3[Int, Int, Int], f: (Int, Int) => Int) =
    ints.productIterator.map(_.toString.toInt).reduce(f)

  /**
    *
    * Q5.4: Let’s say that you happened to run across this function while reviewing another developer’s code:
    * What does this function accomplish? Can you give an example of how you might invoke it?
    *
    * Ans: The function invokes the given function `f` with the argument `x`. It then simply returns `x`.
    * We can assume that `f` has side-effects because it doesn't return anything.
    *
    * The use cases can be varied. `f` could simply be the `println` function of it could be a function that
    * mutates `x`.
    */
  def fzero[A](x: A)(f: A => Unit): A = {
    f(x)
    x
  }

  /**
    * Q5.6: Write a function called “conditional” that takes a value x and two functions, p and f,
    * and returns a value of the same type as x. The p function is a predicate,
    * taking the value x and returning a Boolean b. The f function also takes the value x and returns
    * a new value of the same type. Your “conditional” function should only invoke the function f(x)
    * if p(x) is true, and otherwise return x.
    * How many type parameters will the “conditional” function require?
    */
  private def conditional[A](x: A, p: A => Boolean, f: A => A) =
    if (p(x)) f(x) else x

  private def conditional2[A, B](x: A, y: B, p: A => Boolean, f: A => B) =
    if (p(x)) f(x) else y

  /**
    * Q5.7: There is a popular coding interview question I’ll call "typesafe,"
    * in which the numbers 1-100 must be printed one per line.
    * The catch is that multiples of 3 must replace the number with the word "type,"
    * while multiples of 5 must replace the number with the word "safe."
    * Of course, multiples of 15 must print "typesafe."
    * Use the `conditional` function from exercise 6 to implement this challenge.
    * Would your solution be shorter if the return type of `conditional`
    * did not match the type of the parameter x?
    * Experiment with an altered version of the `conditional` function that works better with this challenge.
    */
  def typesafe = {
    val f: String => String = (str: String) => str match {
      case _ if (str.toInt % 15 == 0) => "typesafe"
      case _ if (str.toInt % 5 == 0) => "type"
      case _ if (str.toInt % 3 == 0) => "safe"
    }

    val p: String => Boolean = (str: String) =>
      (str.toInt % 3 == 0) || (str.toInt % 5 == 0) || (str.toInt % 15 == 0)

    (1 to 100).map(_.toString).map(conditional(_, p, f)).foreach(println)
  }

  def typesafe2 = {
    val f: Int => String = (a: Int) => a match {
      case _ if (a % 15 == 0) => "typesafe"
      case _ if (a % 5 == 0) => "type"
      case _ if (a % 3 == 0) => "safe"
    }

    val p: Int => Boolean = (a: Int) =>
      (a % 3 == 0) || (a % 5 == 0) || (a % 15 == 0)

    (1 to 100).map(a => conditional2(a, a.toString, p, f)).foreach(println)
  }
}
