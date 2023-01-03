/*
* State.scala
* State representation.
* Initial code provided in the book and Solutions related to this section.
* December 30th, 2022
*/

package chapter6

opaque type State[S, +A] = S => (A, S)

object State:
  def apply[S, A](f: S => (A, S)): State[S, A] = f

  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

  // This time my Solutions doesn't have the right level of abstraction.
  // Exercise 6.10 - Generalizing several functions.
  // `unit`
  extension [S, A](underlying: State[S, A])
    def unit(a: A): State[S, A] = State(s => (a, s))

  // `map`
  extension [S, A](underlying: State[S, A])
    def map[B](action: State[S, A], f: A => B): State[S, B] =
      s =>
        val (a, s2) = action.run(s)
        (f(a), s2)

  // `map2`
  extension [S, A](underlying: State[S, A])
    def map2[B, C](actionA: State[S, A], actionB: State[S, B], f: (A, B) => C): State[S, C] =
      s =>
        val (a, s2) = actionA.run(s)
        val (b, s3) = actionB.run(s2)
        (f(a, b), s3)

  // `flatMap`
  extension [S, A](underlying: State[S, A])
    def flatMap[B](action: State[S, A], f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s2) = action.run(s)
        val (b, s3) = f(a)(s2)
        (b, s3)

  // `sequence`
  // Couldn't get this one right.
  // def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
  //   actions.foldRight[State[S, List[A]]](run(State(s => (List.empty[A], s)))) ((action, acc) => map2(action, acc, action :: run(acc)))

  // `sequence` Solution provided in the book.
  //def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    //states.foldRight(unit[S, List[A]](Nil))((s, acc) => s.map2(acc)(_ :: _))

/**
  * This marks the end of Chapter 6 for me.
  *This is a book I certainly will enjoy reading a second time.
**/
