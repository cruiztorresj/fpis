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


object MyLazyList:
	def cons[A](hd: => A, tl: => MyLazyList[A]): MyLazyList[A] =
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)

	def empty[A]: MyLazyList[A] = Empty

	def apply[A](as: A*): MyLazyList[A] =
		if as.isEmpty then empty
		else cons(as.head, apply(as.tail*))