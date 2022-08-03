package chapter3

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import MyAnswersForChapterThree._

class MyAnswersForChapterThreeProperties extends munit.ScalaCheckSuite:

  // Again, having a correct implementation already, will help us to use it as a Test Oracle.
  property("MyList's tail implementation") {
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
  
  property("MyList's setHead implementation") {
    forAll { (ss: List[String]) =>
      if ss.nonEmpty then
        val myList: MyList[String] = MyList(ss*)
        val myListWithNewHead = setHead("Hi!", myList)
        tail(myListWithNewHead) == myList
      else
        true // Again, we are testing for non empty lists only
    }
  }
  
  property("MyList's drop implementation") {
    forAll { (n: Int, xs: List[Int]) =>
      val myList: MyList[Int] = MyList(xs*)
      drop(myList, n) == MyList(xs.drop(n)*)
    }
  }
  
  // Predicate to be used for drop's while implementation
  def isPair(n: Int): Boolean = n % 2 == 0
  
  property("MyList's dropWhile implementation") {
    forAll { (xs: List[Int]) =>
      val myList: MyList[Int] = MyList(xs*)
      dropWhile(myList, isPair) == MyList(xs.dropWhile(isPair)*)
    }
  }
  
  property("MyList's init implementation") {
    forAll { (ss: List[String]) =>
      if ss.nonEmpty then
        val myList: MyList[String] = MyList(ss*)
        init(myList) == MyList(ss.init*)
      else
        true // init is not defined on empty lists, coming back to this later.
    }
  }
  
  property("MyList's length implementation") {
    forAll { (ds: List[String]) =>
      val myList: MyList[String] = MyList(ds*)
      length(myList) == ds.length
    }
  }
  
  // At this point I am wondering if it is safe to call `properties` to the tests we have been writing so far
  // They don't look like properties to me, but statements of true nature.
  property("MyList's sum implementation via foldLeft") {
    forAll { (xs: List[Int]) =>
      val myList: MyList[Int] = MyList(xs*)
      sumViaFoldLeft(myList) == xs.sum
    }
  }
  
  property("MyList's product implementation via foldLeft") {
    forAll { (ds: List[Double]) =>
      val myList: MyList[Double] = MyList(ds*)
      productViaFoldLeft(myList) == ds.product
    }
  }
  
  property("MyList's length implementation via foldLeft") {
    forAll { (ss: List[String]) =>
      val myList: MyList[String] = MyList(ss*)
      lengthViaFoldLeft(myList) == ss.length
    }
  }  
