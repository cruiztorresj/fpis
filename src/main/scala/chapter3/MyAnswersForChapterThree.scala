/**
* MyAnswersForChapterThree.scala
* So these are my answers to the exercises found in chapter three
* Caleb Josue Ruiz Torres
*/

package chapter3

object MyAnswersForChapterThree:
  
  // Exercise 3.1
  // Answer is `3`, Compiler warns about last case being unreachable.

  // Exercise 3.2
  def tail[A](as: MyList[A]): MyList[A] =
    as match
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
      def loop(counter: Int, as: MyList[A]): MyList[A] =
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
    def loop(acc: MyList[A]): MyList[A] =
      acc match
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
    def loop(acc: MyList[A], as: MyList[A]): MyList[A] =
      as match
        case MyList.Nil => MyList.Nil // Most likely an exception should be thrown here
        case MyList.Cons(h, MyList.Nil) => acc
        case MyList.Cons(h, t) => loop(MyList.append(acc, MyList.Cons(h, MyList.Nil)), t)
    
    loop(MyList.Nil, as)
    
  // Exercise 3.7
    
  /* For the time being I don't think product can be implemented in terms of foldRight
  * to halt the recursion when finding zero, you can even try to modify `product` definition
  * in order to first find if there is a zero element in the list and return zero without calling foldRight
  * but in the worst case scenario it will require going through the entire list anyway.
  * For the most general question, short-circuit will need to recurse the entire list.
  */
    
  // Exercise 3.8
  /* Input parameter list is returned
  * I suppose acc becomes the case for the Nil Constructor
  * And `f` becomes the case for the `Cons` constructor
  */
    
  // Exercise 3.9
  def length[A](as: MyList[A]): Int = MyList.foldRight(as, 0, (a, b) => 1 + b)
  
  // Exercise 3.10
  // Please find this solution in MyList's companion object. (ListingThreeDotOne.scala file)
  
  // Exercise 3.11
  // {sum, product, length} via foldLeft
  
  def sumViaFoldLeft(ints: MyList[Int]): Int = MyList.foldLeft(ints, 0, (a, b) => a + b)
  
  def productViaFoldLeft(ds: MyList[Double]): Double = MyList.foldLeft(ds, 1.0, (a, b) => a * b)
  
  def lengthViaFoldLeft[A](as: MyList[A]): Int = MyList.foldLeft(as, 0, (a, b) => 1 + a)
  
  // Exercise 3.12
  def reverse[A](as: MyList[A]): MyList[A] =
    @annotation.tailrec
    def loop[A](acc: MyList[A], as: MyList[A]): MyList[A] =
      as match
        case MyList.Nil => acc
        case MyList.Cons(h, t) => loop(MyList.Cons(h, acc), t)
      
    loop(MyList.Nil, as)
  
  def reverseViaFoldRight[A](as: MyList[A]): MyList[A] =
    MyList.foldRight(as, MyList.Nil: MyList[A], (a, b) => MyList.append(b, MyList.Cons(a, MyList.Nil)))
  
  def reverseViaFoldLeft[A](as: MyList[A]): MyList[A] =
    MyList.foldLeft(as, MyList.Nil: MyList[A], (a, b) => MyList.append(MyList.Cons(b, MyList.Nil), a))
  
  // Exercise 3.13 - (Optional) - Hard
  // Please find this solution in MyList's companion object. (ListingThreeDotOne.scala file)
  
  // Exercise 3.14
  def appendUsingFoldRight[A](as1: MyList[A], as2: MyList[A]): MyList[A] =
    MyList.foldRight(as1, as2, MyList.Cons(_, _))
  
  // Exercise 3.15 - Hard
  /**
  * Please feel free to use `appendUsingFold` to achieve stack-safety.
  * Of course you'll have to modify the aforementioned function to call
  * `foldRightInTermsOfFoldLeft` instead of `foldRight` LoC#146
  */
  def concatenates[A](ass: MyList[MyList[A]]): MyList[A] =
    @annotation.tailrec
    def loop(ass: MyList[MyList[A]], acc: MyList[A]): MyList[A] =
      ass match
        case MyList.Nil => acc
        case MyList.Cons(MyList.Nil, assTail) => loop(assTail, acc)
        case MyList.Cons(as, assTail) => loop(assTail, MyList.append(acc, as))
    
    loop(ass, MyList.Nil)
