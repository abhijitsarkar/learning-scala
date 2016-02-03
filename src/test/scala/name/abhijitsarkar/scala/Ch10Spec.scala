package name.abhijitsarkar.scala

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Abhijit Sarkar
  */
class Ch10Spec extends FlatSpec with Matchers {
  "Ch10" should "order lines starting with right-aligned numbers" in {
    val l = List(" 2 a", " 1 b", " 3 c")

    implicit val orderingByNumber: Ordering[String] = Ordering.by(_.trim.split("\\s")(0).toInt)

    /*
     * Ordering.by has the following signature:
     * def by[T, S](f: T => S)(implicit ord: Ordering[S]): Ordering[T]
     *
     * It takes an implicit Ordering for the function result type, which in this case is String.
     * And the implicit Ordering[String] defined at this point is orderingByNumber.
     * So orderingIgnoringNumber cuts the second words from the strings,
     * and tries to order them using orderingByNumber, which in turn tries to convert them to Int,
     * and throws an exception. To avoid that, explicitly provide the implicit Ordering[String].
     */
    val orderingIgnoringNumber: Ordering[String] = Ordering.by((_: String).trim.split("\\s")(1))(Ordering.String)

    Ch10.sortLines(l) should contain inOrder(" 1 b", " 2 a", " 3 c")

    Ch10.sortLines(l)(orderingIgnoringNumber) should contain inOrder(" 2 a", " 1 b", " 3 c")
  }

  import Ch10.PimpedProduct
  it should "sum the numeric values in a tuple" in {
    ('a', "hi", 2.5, 1, true).sum should be (3.5d)
  }
}
