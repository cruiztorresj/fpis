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
  
  val spaceVarianceInts = Gen.containerOf[Seq, Double](Gen.choose(-1000, 1000))
  
  property("Exercise 4.2  - (Optional) - Variance is non-negative") {
    forAll(spaceVarianceInts) { ds =>
      if ds.isEmpty then
        variance(ds) == MyNone
      else
        val result: Double = variance(ds).getOrElse(-1)
        result >= 0.0
    }
  }
  
  // the above property on its own doesn't give us enough confidence about our `variance` implementation.
  // While it is good to check for non-negative results, for completeness at least the below two properties should be correct.
  
  property("Exercise 4.2  - (Optional) - Variance is invariant with respect to changes in a location parameter") {
    forAll(spaceVarianceInts) { ds =>
      if ds.isEmpty then
        variance(ds) == MyNone
      else
        if ds.length == 1 then
          true // Var(a) = 0 - The Variance of a Constant is zero.
        else
          val dsWithAddedConstant: Seq[Double] = ds.map(_ + 7)
          val dsVariance: Double = variance(ds).getOrElse(-1)
          val dsWithAddedConstantVariance: Double = variance(dsWithAddedConstant).getOrElse(-1)
          math.ceil(dsVariance) == math.ceil(dsWithAddedConstantVariance)
    }
  }
  
  property("Exercise 4.2  - (Optional) - Variance is scaled by the square of that constant") {
    forAll(spaceVarianceInts) { ds =>
      if ds.isEmpty then
        variance(ds) == MyNone
      else
        if ds.length == 1 then
          true // Var(a) = 0 - The Variance of a Constant is zero.
        else
          val constantToScaleBy = 7
          val dsScaledByConstant: Seq[Double] = ds.map(_ * constantToScaleBy)
          val dsVariance: Double = variance(ds).getOrElse(-1)
          val dsScaledByConstantVariance: Double = variance(dsScaledByConstant).getOrElse(-1)
          math.ceil(math.pow(constantToScaleBy, 2) * dsVariance) == math.ceil(dsScaledByConstantVariance)
    }
  }
