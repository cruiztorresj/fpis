/*
* MyLazyList.scala
* Lazy List implementation.
* Code skeleton can be found in the book.
* September 12th, 2022
*/

package chapter5

enum MyLazyList[+A]:
  case Empty
  case Cons(hd: () => A, tl: () => MyLazyList[A])

  def maybeHead: Option[A] =
    this match
      case Empty => None
      case Cons(hd, tl) => Some(hd())

  // Helper method `isEmpty` checks for elements in this lazy list.
  def isEmpty: Boolean =
    this match
      case Empty => true
      case _ => false

  // Exercise 5.1
  def toList: List[A] =
    @annotation.tailrec
    def loop(acc: List[A], theLazyList: MyLazyList[A]): List[A] =
      theLazyList match
        case Empty => acc       
        case Cons(hd, tl) => loop(acc ::: List(hd()), tl())
      
    loop(List.empty[A], this)

  // Exercise 5.2
  // a) Define take(n) for returning the first n elements of a LazyList.
  // def take(n: Int): MyLazyList[A] =
  //  @annotation.tailrec
  //  def loop(acc: MyLazyList[A], lazyList: => MyLazyList[A], n: Int): MyLazyList[A] =
  //    if n == 0 then
  //      acc
  //    else
  //      lazyList match
  //        case Empty => acc
  //        case Cons(hd, tl) => loop (MyLazyList.cons(hd(), acc), tl(), n - 1)
    
  //  if n < 0 then Empty
  //  else loop(Empty, this, n)
  
  // I've had to take a look at the provided answer.
  def take(n: Int): MyLazyList[A] =
    this match
      case Cons(h, t) if n > 1 => MyLazyList.cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => MyLazyList.cons(h(), MyLazyList.empty)
      case _ => MyLazyList.empty

  // Exercise 5.2
  // b) Define drop(n) for skipping thr first n elements of a LazyList.
  def drop(n: Int): MyLazyList[A] =
    if n < 0 || n == 0 then
      this
    else
      @annotation.tailrec
      def loop(counter: Int, result: MyLazyList[A]): MyLazyList[A] =
        if counter == 0 then
          result
        else
          result match
            case Cons(head, tail) => loop(counter - 1, tail())
            case Empty => Empty

      loop(n, this)

  // Exercise 5.3
  // Write a function takeWhile for returning all starting elements of a LazyList
  // that match the given predicate.
  def takeWhile(p: A => Boolean): MyLazyList[A] =
    this match
      case Cons(hd, tl) if p(hd()) => MyLazyList.cons(hd(), tl().takeWhile(p))
      case Cons(hd, tl) if !p(hd()) => MyLazyList.empty
      case _ => MyLazyList.empty

  // Function `exists` that checks whether an element
  // matching a Boolean function exists in this LazyList.
  def exists(p: A => Boolean): Boolean =
    this match
      case Cons(hd, tl) => p(hd()) || tl().exists(p) 
      case _ => false

  // Implementing a general recursion in the form of foldRight      
  def foldRight[B](acc: => B)(f: (A, => B) => B): B =
    this match
      case Cons(hd, tl) => f(hd(), tl().foldRight(acc)(f))
      case _ => acc

  // Using lazy foldRight to implement `exists`
  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.4
  // Implement `forAll`, which checks that all elements in the Lazy List
  // match a given predicate.
  def forAll(p: A => Boolean): Boolean =
    this match
      case Cons(hd, tl) => p(hd()) && tl().forAll(p)
      case _ => true

  // Exercise 5.5
  // Use `foldRight` to implement `takeWhile`
  def takeWhileViaFoldRight(p: A => Boolean): MyLazyList[A] =
    foldRight(MyLazyList.empty)((a, b) => if p(a) then MyLazyList.cons(a, b) else MyLazyList.empty)

  // Exercise 5.6
  // Implemente `headOption` using `foldRight`
  // OK, I am skipping this one since I am not seeing any recursion in `maybeHead` (`headOption`) function.
  // Two weeks later: I think I am giving a shot to this one.
  def headOption: Option[A] = foldRight(Option.empty)((a, b) => Some(a))

  // Exercise 5.7
  // Several functions implementation.

  // `map` implementation using foldRight
  def map[B](g: A => B): MyLazyList[B] =
    foldRight(MyLazyList.empty)((a, b) => MyLazyList.cons(g(a), b))

  // `filter`
  def filter(p: A => Boolean): MyLazyList[A] =
    foldRight(MyLazyList.empty)((a, b) => if p(a) then MyLazyList.cons(a, b) else b)

  // append
  def append[B >: A](bs: => MyLazyList[B]): MyLazyList[B] =
    if bs.isEmpty then
      this
    else
      foldRight(bs)((a, b) => MyLazyList.cons(a, b))

  // flatMap
  def flatMap[B](g: A => MyLazyList[B]): MyLazyList[B] =
    foldRight(MyLazyList.empty)((a, b) => g(a).append(b))

  // Exercise 5.13
  // Several function implementations

  // `map` implementation using `unfold`
  def mapUsingUnfold[B](f: A => B): MyLazyList[B] =
    // OK, in here the second parameter can be rewritten with braces instead of parentheses
    MyLazyList.unfold(this)(_ match
                              case Cons(hd, tl) => Option(f(hd()), tl()) 
                              case _ => None)

  // `take` implementation using `unfold`
  def takeUsingUnfold(n: Int): MyLazyList[A] =
    if n < 0 then
      MyLazyList.empty
    else
      // In here, you can find the second parameter specified within braces instead
      // Of course, this is for pedagogical purposes, in other scenarios you will want to remain consistent
      // With your employer guidelines.
      MyLazyList.unfold((n, this))
        {(counter, theLazyList) =>
          theLazyList match
            case Cons(hd, tl) if counter > 0 => Option(hd(), (counter - 1, tl()))
            case Cons(hd, tl) if counter == 0 => None  
            case _ => None
        }
      // Still, if you want to see how it looks with parentheses. Here you go
      // MyLazyList.unfold((n, this))((counter, theLazyList) =>
      //                              theLazyList match
      //                                case Cons(hd, tl) if counter > 0 => Option(hd(), (counter - 1, tl()))
      //                                case Cons(hd, tl) if counter == 0 => None  
      //                                case _ => None
      //                              )

  // `takeWhile` using `unfold`
  def takeWhileUsingUnfold(p: A => Boolean): MyLazyList[A] =
    MyLazyList.unfold(this)(_ match
      case Cons(hd, tl) if p(hd()) => Option(hd(), tl())
      case Cons(hd, tl) if !p(hd()) => None 
      case _ => None
    )

  // `zipWith` using `unfold`
  def zipWithUsingUnfold[B, C](that: MyLazyList[B], f: (A, B) => C): MyLazyList[C] =
    MyLazyList.unfold(this, that)((thisMyLazyList, thatLMyazyList) =>
      (thisMyLazyList, thatLMyazyList) match {
        case (Cons(ahd, atl), Cons(bhd, btl)) => Option(f(ahd(), bhd()), (atl(), btl()))
        case _ => None
      }
    )

  // `zipAll` using `unfold`
  def zipAll[B](that: MyLazyList[B]): MyLazyList[(Option[A], Option[B])] =
    MyLazyList.unfold(this, that)((thisMyLazyList, thatLMyazyList) =>
      (thisMyLazyList, thatLMyazyList) match
        case (Cons(ahd, atl), Empty) => Option((Some(ahd()), None), (atl(), Empty))
        case (Empty, Cons(bhd, btl)) => Option((None, Some(bhd())), (Empty, btl())) 
        case (Cons(ahd, atl), Cons(bhd, btl)) => Option((Some(ahd()), Some(bhd())), (atl(), btl())) 
        case _ => None
    )

  // Exercise 5.14 (Hard) = Implement `startsWith` using functions you've written
  // OK for this function I have had to check what's the actual behavior for the
  // function with the same name in Scala standard library in order to know what's
  // the expected result when dealing with empty lists.
  def startsWith[A](prefix: MyLazyList[A]): Boolean =
    this.zipAll(prefix).map(zipped =>
      zipped match
        case (Some(a), Some(b)) => a == b
        case (_, None) => true
        case (None, _) => false
    ).forAll(_ == true)

  // Exercise 5.15
  // Implement `tails` using `unfold`
  def tails: MyLazyList[MyLazyList[A]] =
    MyLazyList.unfold(this)(_ match
      case Cons(hd, tl) => Option(Cons(hd, tl), tl())
      case _ => None
    )

  // Exercise 5.16
  // Generalizing `tails` to `scanRight`, returning a Lazy List of intermediate results.
  // In this implementation
  // `LazyList(1, 2, 3).scanRight(0)(_ + _).toList` results in `res0: List[Int] = List(6, 5, 3)`
  // the accumulator itself (0 in this case) is missing, most likely this have been propagated from `tails`
  // While comparing my solutions to the ones found in the book, it is now clear I forgot to append the
  // final empty list at the end in `tails` function.
  // This marks the end of chapter 5.
  def scanRight[B](acc: => B)(f: (A, => B) => B): MyLazyList[B] =
    this.tails.map(_.foldRight(acc)(f))

object MyLazyList:
  def cons[A](hd: => A, tl: => MyLazyList[A]): MyLazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: MyLazyList[A] = Empty

  def apply[A](as: A*): MyLazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  // Provided in the book.
  val ones: MyLazyList[Int] = cons(1, ones)

  // Exercise 5.8
  // Generalize ones slightly to the function `continually`, which returns an
  // Infinite Lazy List of a given value.
  def continually[A](a: A): MyLazyList[A] = cons(a, continually(a))

  // Exercise 5.9
  // Write a function that generates an infinite Lazy List of integers.
  // Starting from n, then n + 1, n + 2, and so on.
  def from(n: Int): MyLazyList[Int] = cons(n, from(n + 1))

  // Exercise 5.10
  // Write a function `fibs` that generates the infinite Lazy List of
  // Fibonnaci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  // OK. In here we already solved fib(n) back in exercise 2.1
  // So, I will just call it.
  // If the author shows how to solve this exercise without an auxiliary
  // Function. I certainly will be amazed.
  def fibs: MyLazyList[Int] =
    def fib(n: Int): Int =
      @annotation.tailrec
      def loop(acc1: Int, acc2: Int, n: Int): Int =
        if n == 1 then acc1
        else if n == 2 then acc2
        else loop(acc2, acc1 + acc2, n - 1)

      loop(0, 1, n)

    def theFibs(n: Int): MyLazyList[Int] = cons(fib(n), theFibs(n + 1))

    theFibs(1)

  // Exercise 5.11
  // Write a more general LazyList-bulding function called `unfold`
  // It takes an initial State, and a function for producing both
  // the next state and the next value in the generated Lazy List.
  def unfold[A, S](state: S)(f: S => Option[(A, S)]): MyLazyList[A] =
    
    /**
     * While the following works, it isn't stack-safe
     * // @annotation.tailrec
     * def loop(results: Option[(A, S)]): MyLazyList[A] =
     *   results match
     *     case Some(a, sNext) => cons(a, loop(f(sNext)))
     *     case _ => empty
     *
     * loop(f(state)) 
     **/

    // This works like a charm too, but I am not sure about its stack-safetyness.
    // Edit: Indeed, it is stack-safe 
    f(state) match
      case Some(a, sNext) => cons(a, unfold(sNext)(f))
      case _ => empty

  // Exercise 5.12
  // Several functions.
  
  // `ones` usinf `unfold`
  val onesUsingUnfold: MyLazyList[Int] = unfold(0)(Option(1, (_: Int)))

  // `continually` using `unfold`
  def continuallyUsingUnfold[A](a: A): MyLazyList[A] = unfold(a)(Option(a, (_: A)))

  // `from` using `unfold`
  def fromUsingUnfold(n: Int): MyLazyList[Int] = unfold(n)(number => Option(number, number + 1))

  // `fibs` using `unfold`
  // here we are again, and of course we are using a helper function.
  def fibsUsingUnfold: MyLazyList[Int] =
    // At this point we have now this function in a three different places. A refactor is required.
    def fib(n: Int): Int =
      @annotation.tailrec
      def loop(acc1: Int, acc2: Int, n: Int): Int =
        if n == 1 then acc1
        else if n == 2 then acc2
        else loop(acc2, acc1 + acc2, n - 1)

      loop(0, 1, n)

    unfold(1)(nth => Option(fib(nth), nth + 1))