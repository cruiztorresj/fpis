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