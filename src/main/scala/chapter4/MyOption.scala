/*
* MyOption.scala
* Re-recreating the Option present in the Scala standard Library for pedagogical purposes
* August 9th, 2022
* Caleb Josue Ruiz Torres
*/

package chapter4

enum MyOption[+A]:
  case MySome(get: A)
  case MyNone
  
  // Exercise 4.1 - Optional
  // Resorting to pattern matching.
  // Even when the authors mention is OK to do this, can't help feeling it's a sort of brute-force approach.
  // Because they also mention that at least three methods can be expressed in terms of `map` and `getOrElse`.
  def map[B](f: A => B): MyOption[B] =
    this match
      case MySome(a) => MySome(f(a))
      case _ => MyNone
  
  /*
  * So, what happened? Why didn't I complete this `flatMap` without resorting to pattern maching?
  * My error was trying to pass exactly a `B` value to `map`.
  * def flatMap[B](f: A => MyOption[B]): MyOption[B] =
  *   this.map(a => f(a).getOrElse(MyNone: MyOption[A]))
  */
  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    this match
      case MySome(a) => f(a)
      case _ => MyNone
     
  def getOrElse[B >: A](default: => B): B =
    this match
      case MySome(b) => b
      case _ => default
  /*
  * Once I saw the answer for `flatMap` in terms of `map` and `getOrElse` provided in the book.
  * I was able to come up with the following solution, but it doesn't feel as good now. Because I've
  * already peek at `flatMap`'s answer.
  * My solution for `orElse` only after seeing the answer for `flatMap`'s.  
  * this.map(a => MySome(a)).getOrElse(ob)
  */
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    this match
      case MyNone => ob
      case _ => this
  /*
  * Again, I was able to come up with this solution only after peeking at `flatMap`'s solution in the book.
  * `if this.map(p).getOrElse(false) then this else MyNone`
  * So I am sticking with the pattern matching.
  * Note, the way they expresses the following computation is quite beauty.
  * This is a book I do recommend.
  */
  def filter(p: A => Boolean): MyOption[A] =
    this match
      case MySome(a) =>
        if p(a) then this
        else MyNone
      case _ => MyNone
  
  // Of course I don't recommend you being so hard with yourself, having a hint when you are stuck is a good thing.
  // I am not providing testing for these methods for the time being.
