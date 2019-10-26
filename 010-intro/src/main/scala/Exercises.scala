// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
//
// Write ITU email addresses of both group members that contributed to
// the solution of the exercise (in lexicographic order).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled inside sbt, using the 'compile' command.
// To run the compiled file use the 'run' or 'runMain' command.
// To load the file int the REPL use the 'console' command.
// Now you can interactively experiment with your code.
//
// Continue solving exercises in the order presented in the PDF file. The file
// shall always compile, run, and pass tests, after you are done with each
// exercise (if you do them in order).  Please compile and test frequently.

// The extension of App allows writing the main method statements at the top
// level (the so called default constructor). For App objects they will be
// executed as if they were placed in the main method in Java.
package fpinscala

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

  // Exercise 3

  // add @annotation.tailrec to make the compiler check that your solution is
  // tail recursive
  def fib (n: Int) : Int = ???

  // Exercise 4

  // add @annotation.tailrec to make the compiler check that your solution is
  // tail recursive
  def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean) :Boolean = ???

  // Exercise 5

  def curry[A,B,C] (f: (A,B)=>C): A => (B => C) = ???

  // Exercise 6

  def uncurry[A,B,C] (f: A => B => C): (A,B) => C = ???

  // Exercise 7

  def compose[A,B,C] (f: B => C, g: A => B) : A => C = ???

  // Exercise 8 requires no programming

  // Exercise 9

  def tail[A] (as: List[A]) :List[A] = ???

  // Exercise 10

  // @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def drop[A] (l: List[A], n: Int) : List[A] = ???

  // Exercise 11

  // @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  // Exercise 12

  def init[A](l: List[A]): List[A] = ???

  // Exercise 13

  def length[A] (as: List[A]): Int = ???

  // Exercise 14

  // @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B): B = ???

  // Exercise 15

  def product (as: List[Int]): Int = ???

  def length1 (as: List[Int]): Int = ???

  // Exercise 16

  def reverse[A] (as: List[A]): List[A] = ???

  // Exercise 17

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B): B = ???

  // Exercise 18

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B): B = ???

  // Exercise 19

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]): List[A] = ???

  // Exercise 20

  def filter[A] (as: List[A]) (f: A => Boolean): List[A] = ???

  // Exercise 21

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = ???

  // Exercise 22

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = ???

  // Exercise 23

  def add (l: List[Int]) (r: List[Int]): List[Int] = ???

  // Exercise 24

  def zipWith[A,B,C] (f: (A,B)=>C) (l: List[A], r: List[B]): List[C] = ???

  // Exercise 25

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = ???

  // Exercise 26

  def pascal (n: Int): List[Int] = ???

}
