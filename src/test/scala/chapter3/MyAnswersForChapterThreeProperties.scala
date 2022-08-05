/**
* MyAnswersForChapterThreeProperties.scala
* Testing properties for answers to chapter 3 exercise solutions.
* Caleb Josue Ruiz Torres
*/

package chapter3

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import MyAnswersForChapterThree._

class MyAnswersForChapterThreeProperties extends munit.ScalaCheckSuite:

  // Again, having a correct implementation already, will help us to use it as a Test Oracle.
  property("Exercise 3.2  - tail implementation") {
    forAll { (xs: List[Int]) =>
      // What's the tail of an empty List?
      // One can argue the tail of Nothing is Nothing, but the book says they will return to this matter at a later point.
      // So we are using nonEmpty lists only
      if xs.nonEmpty then
        val myList: MyList[Int] = MyList(xs*)
        tail(myList) == MyList(xs.tail*)
      else
        true // Most likely modeling a Failure will be better here.
    }
  }
  
  property("Exercise 3.3  - setHead implementation") {
    forAll { (ss: List[String]) =>
      if ss.nonEmpty then
        val myList: MyList[String] = MyList(ss*)
        val myListWithNewHead = setHead("Hi!", myList)
        tail(myListWithNewHead) == myList
      else
        true // Again, we are testing for non empty lists only
    }
  }
  
  property("Exercise 3.4  - drop implementation") {
    forAll { (n: Int, xs: List[Int]) =>
      val myList: MyList[Int] = MyList(xs*)
      drop(myList, n) == MyList(xs.drop(n)*)
    }
  }
  
  // Predicate to be used for drop's while implementation
  def isPair(n: Int): Boolean = n % 2 == 0
  
  property("Exercise 3.5  - dropWhile implementation") {
    forAll { (xs: List[Int]) =>
      val myList: MyList[Int] = MyList(xs*)
      dropWhile(myList, isPair) == MyList(xs.dropWhile(isPair)*)
    }
  }
  
  property("Exercise 3.6  - init implementation") {
    forAll { (ss: List[String]) =>
      if ss.nonEmpty then
        val myList: MyList[String] = MyList(ss*)
        init(myList) == MyList(ss.init*)
      else
        true // init is not defined on empty lists, coming back to this later.
    }
  }
  
  property("Exercise 3.9  - length implementation using foldRight") {
    forAll { (ds: List[String]) =>
      val myList: MyList[String] = MyList(ds*)
      length(myList) == ds.length
    }
  }
  
  // At this point I am wondering if it is safe to call `properties` to the tests we have been writing so far
  // They don't look like properties to me, but statements of true nature.  
  property("Exercise 3.11 - sum implementation using foldLeft") {
    forAll { (xs: List[Int]) =>
      val myList: MyList[Int] = MyList(xs*)
      sumViaFoldLeft(myList) == xs.sum
    }
  }
  
  property("Exercise 3.11 - product implementation using foldLeft") {
    forAll { (ds: List[Double]) =>
      val myList: MyList[Double] = MyList(ds*)
      productViaFoldLeft(myList) == ds.product
    }
  }
  
  property("Exercise 3.11 - length implementation using foldLeft") {
    forAll { (ss: List[String]) =>
      val myList: MyList[String] = MyList(ss*)
      lengthViaFoldLeft(myList) == ss.length
    }
  }
  
  property("Exercise 3.12 - reverse implementation") {
    forAll { (ds: List[Double]) =>
      val myList: MyList[Double] = MyList(ds*)
      reverse(myList) == MyList(ds.reverse*)
    }
  }
  
  property("Exercise 3.12 - (Extra) - reverse implementation using foldRight") {
    forAll { (xs: List[Int]) =>
      val myList: MyList[Int] = MyList(xs*)
      reverseViaFoldRight(myList) == MyList(xs.reverse*)
    }
  }
  
  property("Exercise 3.12 - (Extra) - reverse implementation using foldLeft") {
    forAll { (ss: List[String]) =>
      val myList: MyList[String] = MyList(ss*)
      reverseViaFoldLeft(myList) == MyList(ss.reverse*)
    }
  }
  
  property("Exercise 3.14 - append implementation using foldRight") {
    forAll { (xs: List[Int], ys: List[Int]) =>
      val myListFromXss: MyList[Int] = MyList(xs*)
      val myListFromYss: MyList[Int] = MyList(ys*)
      appendUsingFoldRight(myListFromXss, myListFromYss) == MyList(ys.:::(xs)*)
    }
  }
  
  property("Exercise 3.15 - concatenates implementation") {
    forAll { (intss: List[List[Int]]) =>
      val myIntss: MyList[MyList[Int]] = MyList(intss.map(ints => MyList(ints*))*)
      
      // At first I wanted to use `concat` method as Test Oracle
      // But its definition requires sending parameter lists explicitly
      // So `flatten` will do it.
      concatenates(myIntss) == MyList(intss.flatten*)
    }
  }
