/*
* Solutions.scala
* Solutions to Chapter 6 Exercises.
* Code skeleton can be found in the book.
* December 22th, 2022
*/

package chapter6

trait RNG:
  def nextInt: (Int, RNG)

type Rand[+A] = RNG => (A, RNG)

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)

val randInt: Rand[Int] = _.nextInt

def unit[A](a: A): Rand[A] = rng => (a, rng)

def nonNegativeEven: Rand[Int] = map(nonNegativeInt, i => i - (i % 2))

// Exercise  6.1
def nonNegativeInt(rng: RNG): (Int, RNG) =
  val (numba, nextRNG) = rng.nextInt
  if numba == Int.MinValue then (Int.MaxValue, nextRNG)
  else (Math.abs(numba), nextRNG)

// Exercise 6.2
def double(rng: RNG): (Double, RNG) =
  def getReducedDouble(acc: Int): Double =
    val numberOfDigits: Int = Math.abs(acc).toString.length
    val divisor: Double = Math.pow(10.0, numberOfDigits.toDouble)
    acc / divisor

  val (nextInt, nextRng): (Int, RNG) = rng.nextInt
  (getReducedDouble(nextInt), nextRng)

// Exercise 6.3
def intDouble(rng: RNG): ((Int, Double), RNG) =
  val (theRandomInt, nextRNG) = rng.nextInt
  val (theRandomDouble, lastRNG) = double(nextRNG)
  ((theRandomInt, theRandomDouble), lastRNG)

def doubleInt(rng: RNG): ((Double, Int), RNG) =
  val (theIntAndDouble, nextRNG) = intDouble(rng)
  (theIntAndDouble.swap, nextRNG)

def double3(rng: RNG): ((Double, Double, Double), RNG) =
  val (firstDouble, firstRNG) = double(rng)
  val (secondDouble, secondRNG) = double(firstRNG)
  val (thirdDouble, thirdRNG) = double(secondRNG)
  ((firstDouble, secondDouble, thirdDouble), thirdRNG)

// Exercise 6.4
def ints(count: Int, rng: RNG): (List[Int], RNG) =
  @annotation.tailrec
  def loop(acc: (List[Int], RNG), counter: Int): (List[Int], RNG) =
    if counter == 0 then
      acc
    else
      val (nextInt, nextRng) = acc._2.nextInt
      loop((acc._1.appended(nextInt), nextRng), counter - 1)
	
  loop((List.empty[Int], rng), count)

// Exercise 6.5
def doubleViaMap: Rand[Double] =
  def getReducedDouble(acc: Int): Double =
    val numberOfDigits: Int = Math.abs(acc).toString.length
    val divisor: Double = Math.pow(10.0, numberOfDigits.toDouble)
    acc / divisor
  map(randInt, getReducedDouble)

def map[A, B](s: Rand[A], f: A => B): Rand[B] =
  rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)

// Exercise 6.6
def map2[A, B, C](ra: Rand[A], rb: Rand[B], f: (A, B) => C): Rand[C] =
  rng =>
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb, (_, _))

val randIntDouble: Rand[(Int, Double)] = both(randInt, double)

val randDoubleInt: Rand[(Double, Int)] = both(doubleViaMap, randInt)

// Exercise 6.7 -- Hard
// a)
def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
  rs.foldRight[Rand[List[A]]](unit(List.empty[A]))((theRand, acc) => map2(theRand, acc, _ :: _))

// Here's a version with extreme type inferece, for the time being I am going with the above for pedagogical purposes.
// def ssequence[A](rs: List[Rand[A]]): Rand[List[A]] =
// 	rs.foldRight[Rand[List[A]]](unit(List.empty[A]))(map2(_, _, _ :: _))

// b)
// The type signature have to change since the RNG shouldn't be passed as parameter to this function anymore
def intsAlternateVersion(count: Int): Rand[List[Int]] =
  sequence(List.fill(count)(randInt))

// Exercise 6.8
// a)
def flatMap[A, B](r: Rand[A], f: A => Rand[B]): Rand[B] =
  rng =>
    val (a, rng2) = r(rng)
    val (b, rng3) = f(a)(rng2)
    (b, rng3)

// b)
def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(nonNegativeInt,
    i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod)
      else nonNegativeLessThan(n)
  )

// For appreciation purposes I am transcribing the solution for this problem previous to FlatMap.
// def nonNegativeLessThanNoFlatMap(n: Int): Rand[Int] =
// 	rng =>
// 		val (i, rng2): (Int, RNG) = nonNegativeInt(rng)
// 		val mod: Int = i % n
// 		if i + (n - 1) - mod >= 0 then
// 			(mod, rng2)
// 		else
// 			nonNegativeLessThanNoFlatMap(n)(rng2)

// Exercise 6.9
def mapViaFlatMap[A, B](s: Rand[A], f: A => B): Rand[B] = flatMap(s, a => unit(f(a)))

def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B], f: (A, B) => C): Rand[C] =
  flatMap(ra, a => flatMap(rb, b => unit(f(a, b))))