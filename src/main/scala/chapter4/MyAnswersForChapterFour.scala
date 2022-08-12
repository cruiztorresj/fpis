/* 
* AnswersToExercises.scala
* My solutions for exercises found in Chapter 4.
* August 10th, 2022
* Caleb Josue Ruiz Torres
*/

package chapter4

import MyOption._

object MyAnswersForChapterFour:
  
  // Function provided by the book authors
  def mean(ds: Seq[Double]): MyOption[Double] =
    if ds.isEmpty then MyNone
    else MySome(ds.sum / ds.length)

  // Exercise 4.2 - Optional
  def variance(ds: Seq[Double]): MyOption[Double] =
    mean(ds).flatMap(m => mean(ds.map(d => math.pow(d - m, 2))))
