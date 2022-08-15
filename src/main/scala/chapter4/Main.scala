/* 
* Main.scala
* Entry Point.
* For this chapter I haven't writing proper testing. Instead I ended using this file to experiment with
* input-output for several functions written while solving the exercises. you can see the results by
* means of `sbt run`
* August 12th, 2022
* Caleb Josue Ruiz Torres
*/

package chapter4

import MyOption._
import MyEither._
import MyAnswersForChapterFour._

// @main def m(args: String*) = ??? // In case you want process user arguments

@main def m() =
  val maybeA: MyOption[String] = MySome("Seven")
  val maybeB: MyOption[String] = MySome("Five")
  val emptyMaybe: MyOption[String] = MyNone
  
  // println(MyAnswersForChapterFour.map2(maybeA, maybeB)(f))
  // println(MyAnswersForChapterFour.map2InnerFunctionArgument(maybeA, maybeB, _ + _))
  
  println("All right")
  
  val eitherQuoteA = MyAnswersForChapterFour.getInsuranceRateQuote("25", "4")
  val eitherQuoteB = MyAnswersForChapterFour.getInsuranceRateQuote("30", "5")
  val eitherQuoteC = MyAnswersForChapterFour.getInsuranceRateQuote("50", "2")
  
  println(MyEither.sequence(List(eitherQuoteA, eitherQuoteB, eitherQuoteC)))
  
  println("done")
