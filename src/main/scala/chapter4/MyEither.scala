/* 
* MyEither.scala
* Our Either implementation.
* August 14th, 2022
* Caleb Josue Ruiz Torres
*/

package chapter4

import scala.util.control.NonFatal

enum MyEither[+E, +A]:
  case MyLeft(value: E)
  case MyRight(value: A)
  
  // Exercise 4.6 - Optional.
  // Implement the following four function definitions.
  // Joke: My program type checks, therefore is correct - from edX's FP101
  def map[B](f: A => B): MyEither[E, B] =
    this match
      case MyLeft(failure) => MyLeft(failure)
      case MyRight(result) => MyRight(f(result)) 
  
  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] =
    this match
      case MyLeft(failure) => MyLeft(failure)
      case MyRight(result) => f(result)
  
  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] =
    this match
      case MyRight(result) => this
      case MyLeft(failure) => b
  
  def map2[EE >: E, B, C](that: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    // The following will desugars to `this.flatMap(ea => that.map(eb => f(ea, eb)))`
    for
      ea <- this
      eb <- that
    yield f(ea, eb)
  
object MyEither:
  // Provided in the book
  // `catchNonFatal` factors out the common pattern of converting thrown exceptions to values.
  def catchNonFatal[A](a: => A): MyEither[Throwable, A] =
    try MyRight(a)
    catch case NonFatal(t) => MyLeft(t)
  
  // Exercise 4.7 - Optional
  // Giving up on this one, can't seem to find a proper accumulator to foldRight over the list.
  // This type checks, but I did adapte the thingae from Option's sequence answer
  // Lesson: You can annotate even foldRight!!!
  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
    as.foldRight[MyEither[E, List[B]]](MyRight(Nil)) {
      (a, acc) => f(a).map2(acc)(_ :: _)
    }
  
  def sequence[E, A](as: List[MyEither[E, A]]): MyEither[E, List[A]] = traverse(as)(a => a)
  
  /**
  * Exercise 4.8 - Optional
  * In the summary for this lesson they call `Validated` the data type to be defined for this exercise.
  * Since traverse only reports one failure, even when there is more than one, I suppose this `Validated`
  * Type will in fact require us to create it with the following type signature (more likely).
  * enum Validated ...
  * guess what? This was solved in the EPFL's effective programming with Scala. Enroll now!
  */
