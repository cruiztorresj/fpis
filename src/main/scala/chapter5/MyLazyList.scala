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

	// Exercise 5.1
	def toList: List[A] =
		@annotation.tailrec
		def loop(acc: List[A], theLazyList: MyLazyList[A]): List[A] =
			theLazyList match
				case Empty => acc				
				case Cons(hd, tl) => loop(acc ::: List(hd()), tl())
			
		loop(List.empty[A], this)

	// Exercise 5.2
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

	// Exercise 5.2
	// a) Define take(n) for returning the first n elements of a LazyList.
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

object MyLazyList:
	def cons[A](hd: => A, tl: => MyLazyList[A]): MyLazyList[A] =
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)

	def empty[A]: MyLazyList[A] = Empty

	def apply[A](as: A*): MyLazyList[A] =
		if as.isEmpty then empty
		else cons(as.head, apply(as.tail*))