package name.abhijitsarkar.scala

/**
  * @author Abhijit Sarkar
  */

class Perhaps[A](val a: A) {

}

/**
  * Q10.4: Assume you wrote your own version of Option[A], calling it `Perhaps[A]`,
  * and implemented one or two methods to access its contents.
  * What kind of implicit conversion would you need to provide
  * in order to allow it to be treated as a collection?
  * How would you be able to invoke flatMap and filter on your instance without implementing those methods?
  *
  * Ans: Provide implicit conversion to and from `List[A]`.
  */
object Perhaps {
  def apply[A](a: A) = new Perhaps(a)

  //  implicit class PerhapsPimped[A](val p: Perhaps[A]) {
  //    def map[B](f: A => B): Perhaps[B] = List[A](p.a).map(x => Perhaps(f(x))).head
  //
  //    def get = p.a
  //  }

  implicit def perhapsToList[A](p: Perhaps[A]) = List[A](p.a)

  implicit def listToPerhaps[A](l: List[A]) = Perhaps(l.head)

}
