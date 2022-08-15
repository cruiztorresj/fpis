/* 
* AnswersToExercises.scala
* My solutions for exercises found in Chapter 4.
* August 10th, 2022
* Caleb Josue Ruiz Torres
*/

package chapter4

import MyOption._
import MyEither._

object MyAnswersForChapterFour:
  // From the book, I am going to pretend this exists in a legacy third-party library for pedagogical's purpose sake.
  /**
  * Top secret formula for computing an annual car
  * insurance premium from two key factors.
  */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    0.0 // Of cour this LoC isn't in the book.
  
  // Function provided by the book authors
  def mean(ds: Seq[Double]): MyOption[Double] =
    if ds.isEmpty then MyNone
    else MySome(ds.sum / ds.length)

  // Exercise 4.2 - Optional
  def variance(ds: Seq[Double]): MyOption[Double] =
    mean(ds).flatMap(m => mean(ds.map(d => math.pow(d - m, 2))))
  
  // Exercise 4.3 - Optional
  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    a.flatMap(x => b.map(y => f(x, y)))
  
  def map2InnerFunctionArgument[A, B, C](a: MyOption[A], b: MyOption[B], f: (A, B) => C): MyOption[C] =
    a.flatMap(x => b.map(y => f(x, y)))
  
  // Exercise 4.4 (Optional)
  // This serves better in a companion object for MyOption, for reader's convenience,
  // the mentioned companion object should be declared in MyOption.scala
  def sequence[A](as: List[MyOption[A]]): MyOption[List[A]] =
    as.foldRight(MySome(List.empty[A])) { (oi, acc) =>
      oi.map(i => acc.flatMap(xs => MySome(xs.appended(i)))).getOrElse(MyNone)
    }
  
  // Exercise 4.5 - (Optional)
  def traverse[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    as.foldRight(MySome(List.empty[B])) { (a, acc) =>
      f(a).map(v => acc.flatMap(values => MySome(values.appended(v)))).getOrElse(MyNone)
    }
  
  // part II, sequence in terms of traverse
  // Again, For this exercise. I saw the provided answer in the book. Damn! Again the Type Variable got me confused!
  def sequenceInTraverse[A](as: List[MyOption[A]]): MyOption[List[A]] = ???
  
  // Provided in the book
  def meanAgain(ds: Seq[Double]): MyEither[String, Double] =
    if ds.isEmpty then MyLeft("Empty List")
    else MyRight(ds.sum / ds.length)
  
  // provided in the book
  // I've changed the function's name, you are actually an insuranceRateQuote, but parsing input in the process.
  def getInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): MyEither[Throwable, Double] =
    for
      age <- MyEither.catchNonFatal { age.toInt }
      tickets <- MyEither.catchNonFatal { numberOfSpeedingTickets.toInt }
    // perhaps this call results in an error. Specially if it lives in a Java third-party library where this practice is common.
    yield insuranceRateQuote(age, tickets)
