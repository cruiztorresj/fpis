/**
* MyAnswersForChapterThreeProperties.scala
* Testing properties for answers to chapter 3 exercise solutions.
* Caleb Josue Ruiz Torres
*/

package chapter3

import MyAnswersForChapterThree._
import Tree._

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

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
    // In the end, we are chaining n to be rather a number between this small range
    // Having big n, was causing the test to take up to ten seconds. 
    val smallInteger = Gen.choose(-3, 12)
    
    forAll(smallInteger) { n =>
      forAll { (xs: List[Int]) =>
        val myList: MyList[Int] = MyList(xs*)
        drop(myList, n) == MyList(xs.drop(n)*)   
      }
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
  
  property("Exercise 3.16 - Transforms a list of Integers by adding one to each element") {
    forAll { (ints: List[Int]) =>
      val myList: MyList[Int] = MyList(ints*)
      transformIntsByAddingOne(myList) == MyList(ints.map(_ + 1)*)
    }
  }
  
  property("Exercise 3.17 - Turn each double in a list into its String representation") {
    forAll { (ds: List[Double]) =>
      val myList: MyList[Double] = MyList(ds*)
      transformDoublesIntoStrings(myList) == MyList(ds.map(_.toString)*)
    }
  }
  
  property("Exercise 3.18 - `map` !!!") {
    forAll { (ints: List[Int]) =>
      val myList: MyList[Int] = MyList(ints*)
      map(myList, _.+(1)) == MyList(ints.map(_ + 1)*)
    }
  }
  
  property("Exercise 3.19 - `filter` !!!") {
    val isPair: Int => Boolean = _.%(2) == 0
    
    forAll { (ints: List[Int]) =>
      val myList: MyList[Int] = MyList(ints*)
      filter(myList, isPair) == MyList(ints.filter(isPair)*)
    } 
  }
  
  property("Exercise 3.20 - `flatMap` !!!") {
    val nAndItsSquare: Int => MyList[Int] = n => MyList.Cons(n, MyList.Cons(n * n, MyList.Nil))
    
    forAll { (ints: List[Int]) =>
      val myList: MyList[Int] = MyList(ints*)
      flatMap(myList, nAndItsSquare) == MyList(ints.flatMap(n => List(n, n*n))*)
    }
  }
  
  // This is the same as 3.19 but using `filterUsingFlatMap` instead. Feel free to fet rid of it.
  // In general, you can come up with a refactor for a lot of properties over this file
  property("Exercise 3.21 - filter implementation by means of flatMap") {
    val isPair: Int => Boolean = _.%(2) == 0
    
    forAll { (ints: List[Int]) =>
      val myList: MyList[Int] = MyList(ints*)
      filterUsingFlatMap(myList, isPair) == MyList(ints.filter(isPair)*)
    } 
  }
  
  property("Exercise 3.22 - Accepts two lists and constructs a new list by adding corresponding elements") {
    forAll { (xs: List[Int], ys: List[Int]) =>
      // We are only testing for lists with the same length
      if(xs.length == ys.length) then
        val myListFromXss: MyList[Int] = MyList(xs*)
        val myListFromYss: MyList[Int] = MyList(ys*)
        sumViaFoldLeft(elementWiseListsAddition(myListFromXss, myListFromYss)) == xs.sum + ys.sum
      else
        true
    }
  }
  
  // Feel free to use `zip` function as Test Oracle for this property
  property("Exercise 3.23 - Generalization of the previous function") {
    forAll { () =>
      val myList: MyList[Int] = MyList(List(1, 2, 3)*)
       
      val expectedResult: MyList[Int] = MyList.Cons(1, MyList.Cons(4, MyList.Cons(9, MyList.Nil)))
       
      expectedResult == MyAnswersForChapterThree.
        elementWiseFunctionApplicationOverTwoLists(myList, myList, (x, y) => x * y)
    }
  }
  
  // This is by no means exhaustive, and these things aren't properties any longer. The book isn't about testing anyway
  // And There will be a repo on properties topic.
  property("Exercise 3.24 - Hard - Whether a List contains another List as a subsequence") {
    forAll { () =>
      val sup: MyList[String] = MyList(List("a", "b", "c", "d")*)
      val sub1: MyList[String] = MyList(List("a", "b")*)
      val sub2: MyList[String] = MyList(List("b", "c")*)
      val sub3: MyList[String] = MyList(List("d")*)
      val sub4: MyList[String] = MyList(List()*)
      val notSub1: MyList[String] = MyList(List("x")*)
      val notSub2: MyList[String] = MyList(List("c", "b")*)
      val notSub3: MyList[String] = MyList(List("c", "d", "x")*)
      val notSub4: MyList[String] = MyList(List("x", "a", "b", "c")*)
      
       
      MyAnswersForChapterThree.hasSubsequence(sup, sub1)
      MyAnswersForChapterThree.hasSubsequence(sup, sub2)
      MyAnswersForChapterThree.hasSubsequence(sup, sub3)
      MyAnswersForChapterThree.hasSubsequence(sup, sub4)
      MyAnswersForChapterThree.hasSubsequence(sup, sup)
      !MyAnswersForChapterThree.hasSubsequence(sup, notSub1)
      !MyAnswersForChapterThree.hasSubsequence(sup, notSub2)
      !MyAnswersForChapterThree.hasSubsequence(sup, notSub3)
      !MyAnswersForChapterThree.hasSubsequence(sup, notSub4)
    }
  }
  
  // You get the idea
  property("Exercise 3.25 - `maximum` returns the maximum element in a Tree[Int]") {
    forAll { () =>
      // By our definition a Tree can't be empty.
      val myTree1: Tree[Int] = Leaf(0)
      val myTree2: Tree[Int] = Branch(Branch(Leaf(-4), Leaf(0)), Branch(Leaf(9), Leaf(6)))
       
      myTree1.maximum == 0
      myTree2.maximum == 9
    }
  }
  
  property("Exercise 3.26 - `depth` A tree's maximum path length") {
    forAll { () =>
      val myTree1: Tree[Int] = Leaf(0)
      val myTree2: Tree[String] = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
      val myTree3: Tree[Int] = Branch(Leaf(1), Leaf(3))
      val myTree4: Tree[Int] = Branch(Leaf(7), Branch(Branch(Leaf(1), Leaf(3)), Leaf(9)))
       
      myTree1.depth == 0
      myTree2.depth == 2
      myTree3.depth == 1
      myTree4.depth == 3
    }
  }
  
  property("Exercise 3.27 - `map` over Trees") {
    forAll { () =>
      val myTree1: Tree[Int] = Leaf(0)
      val myTree2: Tree[String] = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
      val myTree3: Tree[Int] = Branch(Leaf(1), Leaf(3))
       
      myTree1.map(_.+(1)) == Leaf(1)
      myTree2.map(_.toUpperCase) == Branch(Branch(Leaf("A"), Leaf("B")), Branch(Leaf("C"), Leaf("D")))
      myTree3.map(_*(5)) == Branch(Leaf(5), Leaf(15))
    }
  }
  
  
