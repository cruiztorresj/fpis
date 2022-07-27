import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import MyAnswersForChapterTwo.{fib, isSorted}

class MyAnswersForChapterTwoProperties extends munit.ScalaCheckSuite:
  val fibonacciDomainForIntDataType: Gen[Int] = Gen.choose(1, 47)
  
  property("nth Fibonacci is equal to the sum of the previous two Fibonacci numbers") {
    forAll(fibonacciDomainForIntDataType.suchThat(_ >= 3)) { (n: Int) =>
      fib(n) == fib(n - 1) + fib(n - 2)
    }
  }
  
  property("Fibonacci numbers are positive") {
    forAll(fibonacciDomainForIntDataType) { (n: Int) =>
      fib(n) > -1
    }
  }
  
  // OK, given the book's exercise 2.2 problem definition.
  // `gt` function will return true only when the first parameter is **greater** than the second parameter
  // so this is OK `List(3, 2, 1)`
  // but this `List(3, 2, 2, 1)` is not.
  // Having `gt` function's type. You are free to include `>=` in your definition, but the problem doesn't mention it.
  // For testing purposes, `Test Oracle` strategy is being used. More information in the below link.
  // https://www.youtube.com/watch?v=Jhzc7fxY5lw
  
  def greaterThan(a: Int, b: Int): Boolean = a > b
  
  property("isSorted with greaterThan function") {
    forAll { (arr: Array[Int]) =>
      val xsFromArrNoDuplicates: List[Int] = arr.toList.distinct // Need to check if `distinct` can be used within the Generator itself.
      val orderedArr = xsFromArrNoDuplicates.sortWith(greaterThan).toArray
      isSorted(orderedArr, greaterThan)
    }
  }
