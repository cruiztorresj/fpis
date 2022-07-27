// A comment!
/* Another comment */
/** A documentation comment */

package chapter2

object MyProgram:
  def abs(n: Int): Int = if n < 0 then -n else n
  
  def factorial(n: Int): Int =
    @annotation.tailrec
    def loop(n: Int, acc: Int): Int =
      if n <= 0 then acc else loop(n - 1, n * acc)
    
    loop(n, 1)
  
  // Monomorphic function to find a string in an array
  def findFirst(ss: Array[String], key: String): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      if n >= ss.length then -1
      else if ss(n) == key then n
      else loop(n + 1)
      
    loop(0)
  
  // Polymorphic function to find the first element that satisfies p
  def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      if n >= as.length then -1
      else if p(as(n)) then n
      else loop(n + 1)
    
    loop(0)
  
  def formatResult(name: String, n: Int, f: Int => Int) =
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  
  // Partial application
  def partial1[A, B, C] (a: A, f: (A, B) => C): B => C = b => f(a, b)
