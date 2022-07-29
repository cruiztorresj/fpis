package chapter3

// Listing 3.1 Singly linked lists
// Now, the listing in the book particularly named this enummeration `List`
enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

object MyList:
  
  // I thought on making this definition tail recursive but at this point of the book we haven't defined tail
  def sum(ints: MyList[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  
  def product(ds: MyList[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  
  def apply[A](as: A*): MyList[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))
