/*
* ListingFourDotOne.scala
* Listing 4.1
* Caleb Josue Ruiz Torres
* August 8th, 2022
*/

package chapter4

def failingFn(i: Int): Int =
  val y: Int = throw new Exception("Epic fail!!!")
  
  try
    val x = 42 + 5
    x + y
  catch
    case e: Exception => 43
