package chapter3

// Listing 3.1 Singly linked lists
// Now, the listing in the book particularly named this enummeration `List`
enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

object MyList:
  
  def sum(ints: MyList[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  
  def sumTailRecursive(ints: MyList[Int]): Int =
  	@annotation.tailrec
  	def loop(acc: Int, ints: MyList[Int]): Int = ints match
  	  case Nil => acc
  	  case Cons(x, xs) => loop(acc + x, xs) 
  	loop(0, ints)
  
  def product(ds: MyList[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  
  def productTailRecursive(ds: MyList[Double]): Double =
    @annotation.tailrec
    def loop(acc: Double, ds: MyList[Double]): Double = ds match
      case Nil => acc
      case Cons(0.0, _) => 0.0
      case Cons(d, ds) => loop(acc * d, ds)
    loop(1.0, ds)
  
  def apply[A](as: A*): MyList[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))
