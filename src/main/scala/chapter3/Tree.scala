/*
* Tree.scala
* Simply Binary Tree code listing presented in the book
* August 7th, 2022
* Caleb Josue Ruiz Torres
*/

package chapter3

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  // Counts the number of nodes (leaves and branches) in a tree
  def size: Int =
    this match
      case Leaf(_) => 1
      case Branch(l, r) => 1 + l.size + r.size
  
  // Exercise 3.26
  // depth
  // Returns the maximum path length from the root of a tree to any leaf.
  // Compare this with the solution provided in the book, those guys are clever.
  def depth: Int =
    this match
      case Leaf(_) => 0
      case Branch(l, r) => (l, r) match
        case (Branch(_, _), Branch(_, _)) => l.depth + r.depth
        case _ => 1 + l.depth + r.depth
  
  // Exercise 3.27
  // map
  def map[B](f: A => B): Tree[B] =
    this match
      case Leaf(i) => Leaf(f(i))
      case Branch(l, r) => Branch(l.map(f), r.map(f))
  
object Tree:
  // Extension method to find the firs positive number in a Tree
  extension (t: Tree[Int]) def firstPositive: Option[Int] =
    t match
      case Leaf(i) => if i > 0 then Some(i) else None
      case Branch(l, r) => l.firstPositive orElse r.firstPositive
  
  // Exercise 3.25
  // maximum
  // Returns the maximum element in a Tree[Int]
  // Note: I am not quite sure if recursion over trees can be optimized with tail recursion.
  extension (t: Tree[Int]) def maximum: Int =
    t match
      case Leaf(n) => n
      case Branch(l, r) => l.maximum.max(r.maximum) // math.max(l.maximum, r.maximum)
  
  // Exercise 3.28
  // Abstract similiraties of the previous three exercises into a more general function `fold`
  /*def fold[A, B](t: Tree[A], acc: B, f: (A, B) => B): B =
    t match
      case Leaf(i) => f(i, acc)
      case Branch(l, r) => (l, r) match
        // case (_, Leaf(ii)) => f(ii, fold(l, acc, f))
        // case (Leaf(ii), _) => f(ii, fold(r, acc, f))
        case (Branch(_, _), _) => fold(l, acc, f)
        case (_, Branch(_, _)) => fold(r, acc, f)
   */
   
   /*def fold[A, B](t: Tree[A], f: A => B): B =
    t match
      case Leaf(i) => f(i)
      case Branch(l, r) => (l, r) match
        // case (_, Leaf(ii)) => f(ii, fold(l, acc, f))
        // case (Leaf(ii), _) => f(ii, fold(r, acc, f))
        case (Branch(_, _), _) => fold(l, acc, f)
        case (_, Branch(_, _)) => fold(r, acc, f)*/
   
   // I couldn't figure out a way to combine the two fuctions at the end. For this problem, I will have to see the answer in the book.
   
   /*
   * OK, Two hours later I'm done going through the answers section.
   * What happened?
   * Use the official repo to assist yourself in your learning journey with this book!!!
   * While the book invites you to write this function, coming up with the signature for this problem doesn't seem to be trivial.
   * The official repo does contains suggested signatures for the problems, however, One can think that having already the type signature
   * Is like having the solution for the problem!!!
   * So, how can you come up for yourself with the beautiful solutions the book provides? I suppose is all about taking enough time.
   * to digest the presented material as they suggest starting the chapter 3.
   * Another think I found out is that I didn't expressed more computations in terms of folds.
   * Perhaps, reading the book twice after some period of time. Is not a bad idea after all.
   */
