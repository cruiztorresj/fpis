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

  property("Exercise 5.4  - LazyList forall implementation") {
    val p: Int => Boolean = _ % 2 == 0

    forAll { (xs: List[Int]) =>
      val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
      myLazyList.forAll(p) == xs.forall(p)
    }
  }

  property("Exercise 5.5  - LazyList takeWhile implementation via foldRight") {
    val p: Int => Boolean = _ % 2 == 0

    forAll { (xs: List[Int]) =>
      val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
      myLazyList.takeWhileViaFoldRight(p).toList == xs.takeWhile(p)
    }
  }

  property("Exercise 5.6  - headOption implementation via foldRight") {
    forAll { (xs: List[Int]) =>
      val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
      myLazyList.headOption == xs.headOption
    }
  }

  property("Exercise 5.7  - a) map implementation over a LazyList") {
    forAll { (xs: List[Int]) =>
      val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
      myLazyList.map(n => n * 2).toList == xs.map(_ * 2)
    }
  }

  property("Exercise 5.7  - b) filter implementation over a LazyList") {
    val p: Int => Boolean = _ % 2 == 0

    forAll { (xs: List[Int]) =>
      val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
      myLazyList.filter(p).toList == xs.filter(p)
    }
  }

  property("Exercise 5.7  - c) append implementation over a LazyList") {
    forAll { (xs: List[Int]) =>
      val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
      myLazyList.append(myLazyList).toList == xs.appendedAll(xs)
    }
  }

  property("Exercise 5.7  - d) flatMap implementation over a LazyList") {
    forAll { (xs: List[Int]) =>
      val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
      myLazyList.flatMap(i => MyLazyList(i, i * 2, i * i)).toList == xs.flatMap(i => List(i, i * 2, i * i))
    }
  }

  property("Exercise 5.8  - continually implementation") {
    val smallInteger = Gen.choose(0, 20)

    forAll(smallInteger) { n =>
      MyLazyList.continually(n).take(n).toList == List.fill(n)(n)
    }
  }

  property("Exercise 5.9  - from implementation") {
    val smallInteger = Gen.choose(0, 20)

    forAll(smallInteger) { n =>
      MyLazyList.from(n).take(n).toList == List.range(n, n + n)
    }
  }

  property("Exercise 5.10 - fibs implementation") {
    val fibos: List[Int] = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181)
    val smallInteger = Gen.choose(0, 20)

    forAll(smallInteger) { n =>
      MyLazyList.fibs.take(n).toList == fibos.take(n)
    }
  }

  property("Exercise 5.12 - unfold ones implementation") {
    val smallInteger = Gen.choose(0, 20)

    forAll(smallInteger) { n =>
      MyLazyList.onesUsingUnfold.take(n).toList == List.fill(n)(1)
    }
  }

  property("Exercise 5.12 - unfold continually implementation") {
    val smallInteger = Gen.choose(0, 20)

    forAll(smallInteger) { n =>
      MyLazyList.continuallyUsingUnfold(n).take(n).toList == List.fill(n)(n)
    }
  }

  property("Exercise 5.12 - unfold from implementation") {
    val smallInteger = Gen.choose(0, 20)

    forAll(smallInteger) { n =>
      MyLazyList.fromUsingUnfold(n).take(n).toList == List.range(n, n + n)
    }
  }

  property("Exercise 5.12 - unfold fibs implementation") {
    val fibos: List[Int] = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181)
    val smallInteger = Gen.choose(0, 20)

    forAll(smallInteger) { n =>
      MyLazyList.fibsUsingUnfold.take(n).toList == fibos.take(n)
    }
  }

  property("Exercise 5.13 - map implementation via unfold") {
    forAll { (xs: List[Int]) =>
      val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
      myLazyList.mapUsingUnfold(_ * 2).toList == xs.map(_ * 2)
    }
  }

  property("Exercise 5.13 - take implementation via unfold") {
    val smallInteger = Gen.choose(-3, 25)

    forAll(smallInteger) { n =>
      forAll { (xs: List[Int]) =>
        val myLazyList: MyLazyList[Int] = MyLazyList(xs*)
        myLazyList.takeUsingUnfold(n).toList == xs.take(n)
      }
    } 
  }