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
  def dropWhile[A](as: MyList[A], p: A => Boolean): MyList[A] = as match
    case MyList.Nil => MyList.Nil
    case MyList.Cons(a, as) =>
      if p(a) then dropWhile(as, p)
      else MyList.Cons(a, as)
  
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
  // WHy can't this function be implemented in constant time like `tail`
  // There's no pointer (Construct) to the list section we are interested in (init).
  
  def init[A](as: MyList[A]): MyList[A] =
    @annotation.tailrec
    def loop[A](acc: MyList[A], as: MyList[A]): MyList[A] = as match
      case MyList.Nil => MyList.Nil // Most likely an exception should be thrown here
      case MyList.Cons(h, MyList.Nil) => acc
      case MyList.Cons(h, t) => loop(MyList.append(acc, MyList.Cons(h, MyList.Nil)), t)
    
    loop(MyList.Nil, as)
    
    // Exercise 3.7
    
    
