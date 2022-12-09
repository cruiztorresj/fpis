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
	// 	@annotation.tailrec
	// 	def loop(acc: MyLazyList[A], lazyList: => MyLazyList[A], n: Int): MyLazyList[A] =
	// 		if n == 0 then
	// 			acc
	// 		else
	// 			lazyList match
	// 				case Empty => acc
	// 				case Cons(hd, tl) => loop (MyLazyList.cons(hd(), acc), tl(), n - 1)
		
	// 	if n < 0 then Empty
	// 	else loop(Empty, this, n)
	
	// I've ended having to take a look at the provided answer.
	// It turns out 
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
	// OK, I am skipping this one since I am not seeing any recursion in `maybeHead` function.

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


object MyLazyList:
	def cons[A](hd: => A, tl: => MyLazyList[A]): MyLazyList[A] =
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)

	def empty[A]: MyLazyList[A] = Empty

	def apply[A](as: A*): MyLazyList[A] =
		if as.isEmpty then empty
		else cons(as.head, apply(as.tail*))

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

	//===============