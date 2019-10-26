// An abstract type for exercise to facilitate our testing setup
// The file also contains the definitions of basic types (List)

package fpinscala

// An ADT of Lists
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }
}

trait ExercisesInterface {

  def fib (n: Int): Int

  def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean) :Boolean

  def curry[A,B,C] (f: (A,B) => C): A => (B => C)

  def uncurry[A,B,C] (f: A => B => C): (A,B) => C

  def compose[A,B,C] (f: B => C, g: A => B): A => C

  def tail[A] (as: List[A]): List[A]

  def drop[A] (l: List[A], n: Int): List[A]

  def dropWhile[A] (l: List[A], f: A => Boolean): List[A]

  def init[A](l: List[A]): List[A]

  def length[A](as: List[A]): Int

  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B): B

  def product (as: List[Int]): Int

  def length1 (as: List[Int]): Int

  def reverse[A] (as: List[A]): List[A]

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B): B

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B): B

  def concat[A] (as: List[List[A]]): List[A]

  def filter[A] (as: List[A]) (f: A => Boolean): List[A]

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]

  def filter1[A] (l: List[A]) (p: A => Boolean): List[A]

  def add (l: List[Int]) (r: List[Int]): List[Int]

  def zipWith[A,B,C] (f: (A,B)=>C) (l: List[A], r: List[B]): List[C]

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean

  def pascal (n: Int): List[Int]
}
