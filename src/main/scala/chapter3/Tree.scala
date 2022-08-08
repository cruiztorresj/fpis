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
  // WIP
  /*def fold[A, B](t: Tree[A], acc: B, f: (A, B) => B): B =
    t match
      case Leaf(i) => f(i, acc)
      case Branch(l, r) => f(fold(l, acc, f), fold(r, acc, f))*/
