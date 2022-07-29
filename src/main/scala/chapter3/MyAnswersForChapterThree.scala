package chapter3

object MyAnswersForChapterThree:
  
  // Exercise 3.1
  // 3

  // Exercise 3.2
  def tail[A](as: MyList[A]): MyList[A] = as match
    case MyList.Nil => MyList.Nil
    case MyList.Cons(a, as) => as
  
  // Exercise 3.3
  def setHead[A](elem: A, as: MyList[A]): MyList[A] = MyList.Cons(elem, as)
  
  // Exercise 3.4
