/**
* ListingThreeDotOne.scala
* Code listing for Chapter three
* Caleb Josue Ruiz Torres
*/

package chapter3

// Listing 3.1
// Singly linked lists.
// Now, the listing in the book particularly named this enummeration `List`
enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

object MyList:
  
  def sum(ints: MyList[Int]): Int =
    ints match
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
  
  def sumTailRecursive(ints: MyList[Int]): Int =
  	@annotation.tailrec
  	def loop(acc: Int, ints: MyList[Int]): Int =
  	  ints match
  	    case Nil => acc
  	    case Cons(x, xs) => loop(acc + x, xs) 
  	  loop(0, ints)
  
  def product(ds: MyList[Double]): Double =
    ds match
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
  
  def productTailRecursive(ds: MyList[Double]): Double =
    @annotation.tailrec
    def loop(acc: Double, ds: MyList[Double]): Double =
      ds match
        case Nil => acc
        case Cons(0.0, _) => 0.0
        case Cons(d, ds) => loop(acc * d, ds)
    
    loop(1.0, ds)
  
  def apply[A](as: A*): MyList[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))
  
  def append[A](as1: MyList[A], as2: MyList[A]): MyList[A] =
    as1 match
      case Nil => as2
      case Cons(h, t) => Cons(h, append(t, as2))
  
  // This isn't in the book but I wanted to make sure about some parameterless function definitions.
  def parameterlessFunctionThatReturnsTheIntSeven: Int = 7
  
  // Listing 3.2
  // Right folds and simple uses.
  def foldRight[A, B](as: MyList[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(h, t) => f(h, foldRight(t, acc, f))
  
  def sumViaFoldRight(ns: MyList[Int]) = foldRight(ns, 0, (x, y) => x + y)
  
  def productViaFoldRight(ds: MyList[Double]) = foldRight(ds, 1.0, _ * _)
  
  // Solution for Exercise 3.10
  /*
  * So the exercise ask us to convince ourselves foldRight isn't tail recursive
  * You can do that by placing `@annotation.tailrec` so the compiler informs you
  * foldRight isn't in tail position, or you can construct an insanely larger list and
  * pass it as parameter.
  * We are convinced!
  * Let's now write `foldLeft` in a tail recursive flavor.
  */
  
  @annotation.tailrec
  def foldLeft[A, B](as: MyList[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)
  
  // Exercise 3.13 - (Optional) - Hard
  // So, my English is broken. For this problem I understood folds need to be implemented based on each other.
  // Let's start with foldRightViaFoldLeft, this way foldRight becomes stack-safe.
  def foldRightViaFoldLeft[A, B](as: MyList[A], acc: B, f: (A, B) => B): B =
    def invertAndCurry(f: (A, B) => B): B => (A => B) = a => b => f(b, a)
    def uncurryInvertedFunction(f: B => A => B): (B, A) => B = (b, a) => f(b)(a)
    
    val invertedCurried = invertAndCurry(f)
    val fInvertedUncurried = uncurryInvertedFunction(invertedCurried)
    foldLeft(as, acc, fInvertedUncurried)
  
  // Use cases for foldRight via foldLeft
  def sumViaFoldRightInTermsOfFoldLeft(ns: MyList[Int]) = foldRightViaFoldLeft(ns, 0, (x, y) => x + y)
  
  def productViaFoldRightInTermsOfFoldLeft(ds: MyList[Double]) = foldRightViaFoldLeft(ds, 1.0, _ * _)
  
  // foldLeftViaFoldRight
  // Inner functions will be redefined here, ig you have time to spare on it.
  // Maybe you can abstract over these redefinitions. (Is up to you)
  def foldLeftViaFoldRight[A, B](as: MyList[A], acc: B, f: (B, A) => B): B =
    def invertAndCurry(f: (B, A) => B): A => (B => B) = b => a => f(a, b)
    def uncurryInvertedFunction(f: A => B => B): (A, B) => B = (a, b) => f(a)(b)
    
    val invertedCurried = invertAndCurry(f)
    val fInvertedUncurried = uncurryInvertedFunction(invertedCurried)
    foldRight(as, acc, fInvertedUncurried)

  // Use case for foldLeft via foldRight
  def lengthViaFoldLeftInTermsOfFoldRight[A](as: MyList[A]): Int = foldLeftViaFoldRight(as, 0, (a, b) => 1 + a)
