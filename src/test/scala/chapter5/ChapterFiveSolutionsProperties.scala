/**
* ChapterFiveSolutionsProperties.scala
* Chapter five solutions tests.
* Caleb Josue Ruiz Torres.
* September 26th, 2022
*/

package chapter5

import MyLazyList._

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class ChapterFiveSolutionsProperties extends munit.ScalaCheckSuite:
  property("Exercise 5.1  - toList implementation") {
    forAll { (xs: List[Int]) =>
      val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
      myLazyList.toList == xs
    }
  }

  property("Exercise 5.2  - LazyList drop implementation") {
    val smallInteger = Gen.choose(-3, 12)

    forAll(smallInteger) { n =>
      forAll { (xs: List[Int]) =>
        val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
        myLazyList.drop(n).toList == xs.drop(n)
      }
    }
  }

  property("Exercise 5.3  - LazyList takeWhile implementation") {
    val p: Int => Boolean = _ % 2 == 0

    forAll { (xs: List[Int]) =>
      val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
      myLazyList.takeWhile(p).toList == xs.takeWhile(p)
    }
  }