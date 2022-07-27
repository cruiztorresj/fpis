/**
* So these are my answers to the exercises found in chapter two
* Caleb Josue Ruiz Torres
*/

package chapter2

object MyAnswersForChapterTwo:
  // Exercise 2.1 (Optional)
  // Recursive function to get the nth fibonacci number
  def fib(n: Int): Int =
    @annotation.tailrec
    def loop(acc1: Int, acc2: Int, n: Int): Int =
      if n == 1 then acc1
      else if n == 2 then acc2
      else loop(acc2, acc1 + acc2, n - 1)
    
    loop(0, 1, n)
  
  
  // Exercise 2.2
  /**
  * isSorted implementation, checks if an Array[A] is sorted
  * according to a given comparation function, gt, which returns
  * true if the first parameter is greater than the second parameter
  */
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    @annotation.tailrec
    def loop(n: Int, acc: Boolean): Boolean =
      if as.length == 0 || as.length == 1 then true
      else
        if n == as.length - 1 then acc
        else loop(n + 1, acc && gt(as(n), as(n + 1)))
    
    loop(0, true)

  //Exercise 2.3
  /*
  * Currying
  * Converts a function f of two arguments into a function
  * of one argument that partially applies f
  */
  
  // OK, in here we are explicitly writing the expected types, however the compiler requites parentheses
  // def curry[A, B, C](f:(A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)
  // In the below version, types are inferred, compiler don't require parameters
  def curry[A, B, C](f:(A, B) => C): A => (B => C) = a => b => f(a, b)
  
  // Exercise 2.4 (Optional)
  /*
  * Implement uncurry, which reverses the transformation of curry (See above).
  * => associates to the right, therefore the following equivalence holds true
  * A => (B => C) ≡ A => B => C
  */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b) // (a, b) => f(a)(b), both version works, type inference is good time in Scala
  
  // Exercise 2.5
  /*
  * Implement the Higher-order function that composes two functions.
  */
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
