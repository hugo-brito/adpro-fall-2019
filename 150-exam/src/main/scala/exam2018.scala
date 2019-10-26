// Name: _____________
// ITU email: ________
package adpro.exam2018

import fpinscala.monoids.Monoid
import fpinscala.monads.Monad
import fpinscala.monads.Functor
import fpinscala.laziness.{Stream,Empty,Cons}
import fpinscala.laziness.Stream._
import fpinscala.parallelism.Par._
import scala.language.higherKinds
import adpro.data._
import adpro.data.FingerTree._
import monocle.Lens

object Q1 { 

  def groupByKey[K,V] (l :List[(K,V)]) :List[(K,List[V])] = ???

}


object Q2 { 

  def f[A,B] (results: List[Either[A,B]]) :Either[List[A],List[B]] = ???

}


object Q3 {

  type T[B] = Either[String,B]
  implicit val eitherStringIsMonad :Monad[T] = ???



  implicit def eitherIsMonad[A] = {
    type T[B] = Either[A,B]
    ??? 
  }

} // Q3


object Q4 {

   // Write the answers in English below.
   
   // A. ...
   
   // B. ...
   
}


object Q5 { 

  def parForall[A] (as: List[A]) (p: A => Boolean): Par[Boolean] = ???

}


object Q6 {

  def apply[F[_],A,B](fab: F[A => B])(fa: F[A]): F[B] = ???
  def unit[F[_],A](a: => A): F[A] = ???

  val f: (Int,Int) => Int = _ + _
  def a :List[Int] = ???

  // Answer below in a comment:

  // ...

} // Q6


object Q7 {

  def map2[A,B,C] (a :List[A], b: List[B]) (f: (A,B) => C): List[C] = ???


  def map3[A,B,C,D] (a :List[A], b: List[B], c: List[C]) (f: (A,B,C) => D) :List[D] = ???


  // def map3monad ...

} // Q7


object Q8 {

  def filter[A] (t: FingerTree[A]) (p: A => Boolean): FingerTree[A] = ???

}


object Q9 {

  def eitherOption[A,B] (default: => A): Lens[Either[A,B],Option[B]] = ???


  // Answer the questions below:

  // A. ...

  // B. ...

  // C. ...

} // Q9

