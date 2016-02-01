package name.abhijitsarkar.scala

import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.{universe => ru}

/**
  * @author Abhijit Sarkar
  */
class MyListSpec extends FlatSpec with Matchers {
  "MyList" should "instantiate" in {
    val inputAndOutput = Table(
      ("ip", "op"),
      (Nil, MyNil[Int]),
      (List(1), Cons[Int](1, MyNil[Int])),
      (List(1, 2), Cons[Int](1, Cons[Int](2, MyNil[Int]))),
      (List(1, 2, 3), Cons[Int](1, Cons[Int](2, Cons[Int](3, MyNil[Int]))))
    )

    forAll(inputAndOutput) { (ip, op) =>
      MyList(ip: _ *) should equal(op)
    }
  }

  "MyList" should "print all elements" in {
    MyList(Nil: _ *).foreach(println)
    MyList(List(1, 2, 3): _ *).foreach(println)
  }

  "MyList" should "return head element" in {
    val inputAndOutput = Table(
      ("ip", "op"),
      (List(1), 1),
      (List(1, 2), 1),
      (List(1, 2, 3), 1)
    )

    forAll(inputAndOutput) { (ip, op) =>
      MyList(ip: _ *).head should be(op)
    }
  }

  "MyList" should "throw exception if head method is called on an empty list" in {
    intercept[NoSuchElementException] {
      MyList[Int]().head
    }
  }

  "MyList" should "return tail" in {
    val inputAndOutput = Table(
      ("ip", "op"),
      (List(1), MyNil[Int]),
      (List(1, 2), Cons[Int](2, MyNil[Int])),
      (List(1, 2, 3), Cons[Int](2, Cons[Int](3, MyNil[Int])))
    )

    forAll(inputAndOutput) { (ip, op) =>
      MyList(ip: _ *).tail should equal(op)
    }
  }

  "MyList" should "throw exception if tail method is called on an empty list" in {
    intercept[UnsupportedOperationException] {
      MyList[Int]().tail
    }
  }

  "MyList" should "filter to keep only even numbers" in {
    val inputAndOutput = Table(
      ("ip", "op"),
      (List(1), MyNil[Int]),
      (List(1, 2), Cons[Int](2, MyNil[Int])),
      (List(1, 2, 3), Cons[Int](2, MyNil[Int])),
      (List(1, 2, 3, 4), Cons[Int](2, Cons[Int](4, MyNil[Int])))
    )

    forAll(inputAndOutput) { (ip, op) =>
      MyList(ip: _ *).filter(_ % 2 == 0) should equal(op)
    }
  }

  "MyList" should "double all elements" in {
    val inputAndOutput = Table(
      ("ip", "op"),
      (List(1), Cons[Int](2, MyNil[Int])),
      (List(1, 2), Cons[Int](2, Cons[Int](4, MyNil[Int])))
    )

    forAll(inputAndOutput) { (ip, op) =>
      MyList(ip: _ *).map(_ * 2) should equal(op)
    }
  }

  "MyList" should "return size" in {
    val inputAndOutput = Table(
      ("ip", "op"),
      (Nil, 0),
      (List(1), 1),
      (List(1, 2), 2)
    )

    forAll(inputAndOutput) { (ip, op) =>
      MyList(ip: _ *).size should be(op)
    }
  }

  def equal(right: MyList[Int]) = new MyListMatcher(right)
}

class MyListMatcher(val right: MyList[Int]) extends Matcher[MyList[Int]] {
  override def apply(left: MyList[Int]): MatchResult = {
    MatchResult(
      isEqual(left, right),
      s"""MyList $left did not match "$right"""",
      s"""MyList $left matched "$right""""
    )
  }

  def isEqual(left: MyList[Int], right: MyList[Int]): Boolean = {
    left match {
      case _: MyNil[Int] if right.isInstanceOf[MyNil[Int]] => println("Both are Nil."); true
      case Cons(h, t) if right.isInstanceOf[Cons[Int]] => {
        println("Both are Cons.")
        val r = right.asInstanceOf[Cons[Int]]

        h == r.h && isEqual(t, r.t)
      }
      case _ => println("No match."); false
    }
  }
}
