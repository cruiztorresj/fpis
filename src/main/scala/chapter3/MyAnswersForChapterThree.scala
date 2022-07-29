package chapter3

object MyAnswersForChapterThree:
  
  // Exercise 3.1
  // Answer is `3`, the compiler warns about an unreachable case statement anyway.

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
      def loop(counter: Int, as: MyList[A]): MyList[A] =
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
