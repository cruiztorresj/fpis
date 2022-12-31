/*
* State.scala
* State representation.
* Code skeleton can be found in the book.
* December 30th, 2022
*/

package chapter6

opaque type State[S, +A] = S => (A, S)

object State:
  def apply[S, A](f: S => (A, S)): State[S, A] = f

  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

  // Exercise 6.10 - Generalizing several functions.
  // `unit`
  extension [S, A](underlying: State[S, A])
    def unit(a: A): State[S, A] = s => (a, s)