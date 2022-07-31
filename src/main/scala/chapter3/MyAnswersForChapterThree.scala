/**
* So these are my answers to the exercises found in chapter three
* Caleb Josue Ruiz Torres
*/

package chapter3

object MyAnswersForChapterThree:
  
  // Exercise 3.1
  // Answer is `3`, Compiler warns about last case being unreachable.

  // Exercise 3.2
  def tail[A](as: MyList[A]): MyList[A] = as match
    case MyList.Nil => MyList.Nil
    case MyList.Cons(a, as) => as
  
  // Exercise 3.3
  def setHead[A](elem: A, as: MyList[A]): MyList[A] = MyList.Cons(elem, as)
  
  // Exercise 3.4
  /*
  * OK, this test takes its time to be executed (I am talking on seconds here), note that this function is tail recursive.
  * We need to discard the `tail` function cal, since `tail` operation takes constant time to be performed.
  * so, what I think is happening is the fact that I have seen big numbers being generated for the `n` parameter
  * And our function goes all the way from that big numbers up to 1, our counter.
  * We can opt in to generate over a defined range, say (1, 100) and our list parameter not greater that 100
  * But I'd like the way the actual concrete test cases are generated anyway.
  */
  def drop[A](as: MyList[A], n: Int): MyList[A] =
    if n < 1 then
      as
    else
      @annotation.tailrec
      def loop[A](counter: Int, as: MyList[A]): MyList[A] =
        if counter == 1 then as
        else loop(counter - 1, tail(as))
      
      as match
        case MyList.Nil => MyList.Nil
        case MyList.Cons(a, as) => loop(n, as)
  
  // Exercise 3.5
  /*
  * The following is the first attempt at solving `dropWhile`
  * But it isn't what they call: Stack-safe, this form of recursion is visually clear however.
  * Then we decided to go tail recursive one more time (See uncommented implementation)
  
  def dropWhile[A](as: MyList[A], p: A => Boolean): MyList[A] = as match
    case MyList.Nil => MyList.Nil
    case MyList.Cons(a, as) =>
      if p(a) then dropWhile(as, p)
      else MyList.Cons(a, as)
  */
  
  def dropWhile[A](as: MyList[A], p: A => Boolean): MyList[A] =
    @annotation.tailrec
    def loop(acc: MyList[A]): MyList[A] = acc match
      case MyList.Nil => MyList.Nil
      case MyList.Cons(h, t) =>
        if p(h) then loop(t)
        else acc
    
    loop(as)
  
  // Exercise 3.6
  /*
  * First attempt to define `init` method, coincidentally we found out this
  * definition could serve as basis for a `reverse` method.
  def init[A](as: MyList[A]): MyList[A] =
    @annotation.tailrec
    def loop[A](acc: MyList[A], as: MyList[A]): MyList[A] = as match
      case MyList.Nil => MyList.Nil
      case MyList.Cons(h, MyList.Nil) => acc
      case MyList.Cons(h, t) => loop(MyList.Cons(h, acc), t)
      
    loop(MyList.Nil, as)
  */
  
  // being `append` method just defined previously to this exercise, we use it here.
  // Why can't this function be implemented in constant time like `tail`
  // There's no pointer (Construct) to the list section we are interested in (init).
  
  def init[A](as: MyList[A]): MyList[A] =
    @annotation.tailrec
    def loop[A](acc: MyList[A], as: MyList[A]): MyList[A] = as match
      case MyList.Nil => MyList.Nil // Most likely an exception should be thrown here
      case MyList.Cons(h, MyList.Nil) => acc
      case MyList.Cons(h, t) => loop(MyList.append(acc, MyList.Cons(h, MyList.Nil)), t)
    
    loop(MyList.Nil, as)
    
    // Exercise 3.7
    
    
