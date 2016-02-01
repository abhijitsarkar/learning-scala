package name.abhijitsarkar.scala

import scala.collection.mutable.{Builder => ListBuilder}

/**
  * @author Abhijit Sarkar
  */
/**
  * Q8.2: Create a linked list, object-oriented-style.
  *
  * Q8.2a: Create a container class that has an instance of itself plus an instance of a parameterized type.
  * The constructor should take a variable number of the instances
  * (e.g., strings or ints or any other parameterized type),
  * which can be implemented with ''vararg parameters''.
  * Implement a `foreach` method that users can call to iterate over the list,
  * invoking their function for every element.
  * - How will you determine the end of the list?
  * - C-style lists often use a null value to denote the end of the list. Is that the best approach here?
  * Do you have a good use for the `apply()` method here?
  *
  * Q8.2b: I’m sure your linked list works great, but let’s try refactoring it
  * with a more interesting approach. Make your container class abstract with two subclasses:
  * one representing a node with a valid item and one representing a node without a valid item,
  * signifying the last item in the list.
  * - Will you ever need more than one instance of the second subclass?
  * - Are there any helper methods that should be private?
  * - How about abstract methods that the subclasses will need to implement?
  * - If you implemented the `apply()` method, should each subclass have its own implementation?
  *
  * Q8.2c: Add the standard `head`, `tail`, `filter`, `size`, and `map` collection methods for your linked list.
  * Can you implement any of these using lazy values?
  * Which of these should be implemented in the parent class versus being implemented in its sub-classes?
  *
  * Q8.2d: Implement the `head`, `tail`, `filter`, `size`, and `map` collection methods using recursion
  * instead of iteration. Can you ensure these all use tail recursion
  * to prevent stack overflow errors for massive collections?
  *
  * Ans:
  * Instead of iteratively modifying the program as asked by the above questions, I developed one version
  * for all questions.
  * - The end of the list of indicated by the `MyNil` subclass.
  * - Using a null value is not consistent with the Scala idiom because the methods `filter`, `map`, and `size`
  * when invoked on an empty `MyList` instance should not fail with a NPE, but return an empty list in the
  * 1st 2 cases, and 0 in the last. Similarly methods `head` and `tail` throw exceptions but not NPE.
  *
  * - There is no need for more than one instance of the `MyNil` subclass.
  * - There are no helper methods so no question of making them private.
  * - The abstract class `MyList` provides default implementations for all methods, consistent with
  * an empty list. The subclass `Cons` overrides all methods to provide alternative implementations.
  * - I did implement `apply` for the abstract class and for the `MyNil` subclass. The `Cons` subclass
  * does not need an `apply` method as clients should not instantiate it directly, but use the `MyList.apply`
  * instead.
  *
  * - I do not see any benefit in implementing the methods as lazy values. Those are not using any
  * resources unless called.
  * - Answered above.
  *
  * - All methods are implemented using tail recursion.
  */
sealed abstract class MyList[A] {
  def foreach(f: A => Unit) = {}

  def head: A = throw new NoSuchElementException("head of empty list")

  def tail: MyList[A] = throw new UnsupportedOperationException("tail of empty list")

  def filter(p: A => Boolean): MyList[A] = MyNil[A]

  def map[B](f: A => B): MyList[B] = MyNil[B]

  def size: Int = 0
}

sealed class MyNil[A] extends MyList[A]

sealed case class Cons[A](h: A, t: MyList[A]) extends MyList[A] {
  override def foreach(f: A => Unit) = {
    @annotation.tailrec
    def loop(tail: MyList[A]): Unit =
      tail match {
        case Cons(h, t) => f(h); loop(t)
        case _ =>
      }

    f(h);
    loop(t)
  }

  override def head: A = h

  override def tail: MyList[A] = t

  override def filter(p: A => Boolean) = {
    val buffer = List.newBuilder[A]

    @annotation.tailrec
    def loop(tail: MyList[A]): ListBuilder[A, List[A]] =
      tail match {
        case Cons(h, t) => if (p(h)) buffer += h; loop(t)
        case _ => buffer
      }

    if (p(h)) buffer += h

    val filtered = loop(t)

    /* '_ *' converts a list to a varargs args. */
    MyList(filtered.result(): _ *)
  }

  override def map[B](f: A => B) = {
    val buffer = List.newBuilder[B] += f(h)

    @annotation.tailrec
    def loop(tail: MyList[A]): ListBuilder[B, List[B]] =
      tail match {
        case Cons(h, t) => buffer += f(h); loop(t)
        case _ => buffer
      }

    /* '_ *' converts a list to a varargs args. */
    MyList(loop(t).result(): _ *)
  }

  override def size = {
    var s: Int = 1

    @annotation.tailrec
    def loop(tail: MyList[A]): Int =
      tail match {
        case Cons(h, t) => s += 1; loop(t)
        case _ => s
      }

    loop(t)
  }
}

object MyNil {
  def apply[A] = new MyNil[A]
}

object MyList {
  def apply[A](items: A*): MyList[A] = {
    items match {
      /* '_ *' converts a list to a varargs args. */
      case h :: t => Cons[A](h, apply(t: _ *))
      case _ => MyNil[A]
    }
  }
}
