/* 
* AnswersToExercisesProperties.scala
* Testing solutions for Chapter 4 exercises. 
* August 11th, 2022
* Caleb Josue Ruiz Torres
*/

package chapter4

import MyOption._
import MyAnswersForChapterFour._

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class MyAnswersForChapterFourProperties extends munit.ScalaCheckSuite:
  // Variance function will be tested with properties enummerated at
  // https://en.wikipedia.org/wiki/Variance#Definition
  property("Exercise 4.2  - (Optional) - Variance is non-negative") {
    forAll { (ds: Seq[Double]) =>
      if ds.isEmpty then
        variance(ds) == MyNone
      else
        val result: Double = variance(ds).getOrElse(-1)
        result >= 0.0
    }
  }
