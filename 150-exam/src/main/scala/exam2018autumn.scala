/************************************************************************
  Final Exam: Advanced Programming (Master-Level, MSc-Level)
  IT University of Copenhagen, Autumn 2018: 9 January 9:00
  Andrzej Wąsowski
 ************************************************************************

  Your Full Name: ___
  Your ITU email account: ___

  The exam consists of 10 tasks to  be solved within 4 hours.  MSc and
  Master students  (so all  registered) solve the  same exam,  all the
  tasks.   You  can  use  any  function  from  the  course  (textbook,
  exercises) in the solutions, as  well as standard library functions,
  unless stated otherwise in the question.  You can access any written
  or electronic  materials, also  online, but you  are not  allowed to
  communicate with anybody during the exam.

  By  submitting,  you declare  to  have  solved the  problems  alone,
  without communicating with anybody.



  SUBMISSION

  Solve the tasks in the file 'exam2018autumn.scala' (this file) found
  in the zip archive made available on LearnIt.

  Fill in your name and your ITU email above, in the top of the file.

  Submit this file  and only this file to learnIT.   Do not convert it
  to  any other  format than  .scala.  Do  not submit  the entire  zip
  archive. Do  not reorder  the answers,  and do  not remove  question
  numbers from the  file.  When free text answers  are expected, write
  them as comments.

  The only accepted file format is '.scala'.

  The  answers  will   be  graded  manually. We  will   focus  on  the
  correctness of ideas and the use  of the course concepts. We will be
  permissive on  minor issues  such as semicolons,  other punctuation,
  small deviations  in function  names, switching between  curried and
  uncurried  parameters, etc.   We  will not  check  whether the  type
  inference  succeeds. It suffices  that  a human  reader could  infer
  types.



  REMARKS

  Each  task has  some introduction  text. The part  that needs  to be
  solved is marked with the letter 'Q'.

  We do not recommend solving questions to the point when they compile
  and pass tests.  Dependency problems  and other technical issues can
  take a lot of time, so only do this, once you are done with drafting
  all answers.

  Nevertheless, if  you do compile,  you can use the  'build.sbt' file
  provided  in the  zip  archive linked  above. It  has the  necessary
  library dependencies  configured. The file also contains  the course
  libraries that the solutions depend on.

  Some values are made lazy  so prevent your executions failing before
  all exercises are solved (this is  too help you running in the REPL,
  or  testing otherwise.   Otherwise  these lazy  annotations have  no
  addition meaning.

  Good luck!

************************************************************************1G*/

package adpro

import fpinscala.monoids.Monoid
import fpinscala.monads.Monad
import fpinscala.monads.Functor
import fpinscala.laziness.{Stream,Empty,Cons}
import fpinscala.laziness.Stream._
import fpinscala.parallelism._
import fpinscala.parallelism.Par._
import scala.language.higherKinds
import adpro.data._
import adpro.data.FingerTree._
import monocle.Lens

import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import Gen._

object Exam2018Autumn {


  object Vector {

    /**
     *  Task 1. VECTORS.
     *
     *  We are interested in implementing a type of vectors of double numbers. We
     *  decided that we will represent these vectors as lists of doubles wrapped
     *  in an option, so that we can capture failure of vector operations:
     */

    type VectorD = Option[List[Double]]

    /* Q. Implement a sum for vector operations: the sum of vectors 'v1' and 'v2'
     * is the vector, say 'vector_plus', that at position 'i' has the sum of the
     * values from 'v1' and 'v2' at the same position:
     *
     * vector_plus(i) = v1(i) + v2(i).
     *
     * The operation shall fail, if the two vectors are of different size.
     *
     * For full points exploit the fact that Option is a monad, and don't use
     * pattern matching in your solution.
     */

    def vector_plus (v1: VectorD, v2: VectorD) = ???




    /* Task 2. PROPERTY TESTING
     *
     * Q. Write a property test that establishes the correctness of
     * vector_plus (that for vectors of the same lengths the value at the ith
     * position equals to the sum of ith positions of the summed vectors).
     *
     * Use the following generator 'genVV' of paired vectors of the same size.
     */

    class VectorSpec extends FreeSpec with Checkers {

      "vector_plus is pointwise correct" in check {

        val genVV: Gen[(VectorD,VectorD)] =
          for {
            n <- Gen.choose(0,10000)
            v1 <- Gen.listOfN (n,arbitrary[Double])
            v2 <- Gen.listOfN (n,arbitrary[Double])
          } yield (Some(v1) -> Some(v2))

        /* Write your test below: */

        // forAll  ...

      }
    }
  }




  /* Task 3. CURRYING
   *
   * Consider the following function 'fn':
   */
  val fn = (f :(Int,Int)=> (Int => Int), g: Int=>Int) => (a:Int, b:Int, c:Int) => f(g(a),g(b)) (c)

  /* Recall the curry operation on functions (no need to write the body) */

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = ???

  /* What is the type of expression value 'curry (fn)' ?
   * Q. Replace 'Any' below with the most specific type: */

  lazy val fn_curried = curry (fn): Any

  /* Q. Explain in English why the above is the answer: in what way the type of
   * fn is transformed ?
   *
   *
   *
   *
   *
   */




  /* Task 4.  DEFINING STREAMS
   *
   * The following is an infinite series known as Leibniz sequence:
   *
   *     s(k) = 4 * ((-1)^k) / (2k+1) ,
   *
   * where 'k' is the index of elements of the sequence (a natural number,
   * ranging from 1 to infinity), and 'x^y' denotes exponentiation. The division
   * is a floating point division, and we need to produce a floating point
   * number, not an integer.
   *
   * Q. Define a lazy stream whose k'th element equals s(k), starting with s(0)
   * in the head.
   */

  lazy val leibniz_stream: Stream[Double] = ???




  /* Task 5. OPERATIONS ON STREAMS
   *
   * Assume that leibniz_stream from Task 4 is defined, even if you failed to solve
   * the previous task.  This stream has the following interesting property:
   * the 'Pi' number is approximated by summing a prefix of it.   For
   * instance the following gives a crude approximation of 'Pi':
   *
   *   val pi10000 = leibniz_stream.take (10000).toList.sum
   *
   * Longer prefix you sum, better approximation you obtain.
   *
   * Q. Define an infinite stream of doubles that contains increasingly better
   * approximations of Pi, starting with the first element of leibniz_stream and
   * containing the sums of prefixes of leibniz stream in subsequent elements.
   */

  lazy val pi_stream = ???




  /* Task 6. EITHER
   *
   * Q. Implement function flatMap2 that given two values of Either over
   * the same left type (error type) and two different value types (right)
   * merges them using a binary function:
   */

	def flatMap2[A,B,C,D] (a: Either[D,A], b: Either[D,B]) (f: (A,B) => Either[D,C]): Either[D,C] = ???




  /* Task 7. MONADS, TYPE-CLASSES & HIGHER-KINDS
   *
   * Q. Implement function flatMap2 that given two values of Monad M over
   * the same left type (error type) and two different value types (right)
   * merges them using a binary function.
   *
   * The type signature have been sketched for you below, but note that you
   * will have to modify it to ensure that an instance of Monad[M] exists, in
   * order to access it.
   */

	def flatMap2 [M[_],A,B,C] (a: M[A], b: M[B]) (f: (A,B) => M[C]): M[C] = ???




  /* Task 8.  PAR & CALL-BY-NAME
   *
   * Q. Implement a function
   *
   *     def parEqual (a: =>Object, b: =>Object) :Par[Boolean]
   *
   * that evaluates its arguments in parallel and returns true iff they are
   * equal.
   */

  def parEqual (a: =>Object, b: =>Object) :Par[Boolean] = ???

  /* Q. Is it important that the arguments are passed by name?
   *
   * Answer: ...
   *
   *
   *
   *
   */





  /* Task 9.  DEQUES & PERSISTENT DATA STRUCTURES
   *
   * Assume we have an abstract interface Deque:
   *
   *  trait Deque[+A] { def addL[B >:A] (b: B) :Deque[B] def addR[B >:A] (b: B)
   *  :Deque[B] def empty: Boolean def nonEmpty: Boolean def headL :A def tailL
   *  :Deque[A] def headR :A def tailR :Deque[A] }
   *
   *  This interface is implemented by our FingerTrees in the course (and the
   *  FingerTrees module in this zip bundle has been slightly adjusted to make
   *  this interface explicit).
   *
   *  We want to obtain a new Deque implementation by combination: it takes two
   *  deques and presents them as a single one. For example it takes two Finger
   *  Trees and presents them as a single one.  The idea is to interleave the
   *  two incoming deques, taking elements from them, or putting into them in
   *  alternation.   So if you insert elements in sequence on the left, they are
   *  put interchangebly either in the first or the second combined deque.
   *  Similarly for removal.
   *
   *  For this we need to remember which one of the aggregated deques is active.
   *  We store it in the third parameter of the combinator constructor below
   *  (this.first_active). Initially the first argument deque is active,by
   *  default.
   *
   *  Study the implementation below until you reach the question text.
   */

  case class CombineDeque[+A] (t1: Deque[A], t2: Deque[A], first_active: Boolean = true) extends Deque[A] {

    // A combined deque is empty iff both arguments are empty
    def empty = t1.empty && t2.empty
    def nonEmpty = ! empty

    def headL = (t1.empty, t2.empty, first_active) match {

      // If both t1, t2 empty, fail on the active one
      case (true,true,true)       => t1.headL
      case (true,true,false)      => t2.headL

      // If t1 active and nonempty, or if t2 empty return the head of t1
      case (false,false,true)  => t1.headL
      case (false,true,_)      => t1.headL

      // If t2 active and nonempty, or if t1 empty return the head of t2
      case (false,false,false) => t2.headL
      case (true,false,_)      => t2.headL
    }

    def tailL :Deque[A] = (t1.empty,t2.empty,first_active) match {

      // if both t1 t2 empty, then fail on the right one
      case (true,true,true)    => CombineDeque (t1.tailL,t2,!first_active)
      case (true,true,false)   => CombineDeque (t1,t2.tailL,!first_active)
      //
      // if t1 nonempty and active, or if t2 empty, take the tail from t1
      case (false,false,true)  => CombineDeque (t1.tailL,t2,!first_active)
      case (false,true,_)      => CombineDeque (t1.tailL,t2,!first_active)

      // if t2 nonempty and active, or if t1 empty, take the tail from t2
      case (false,false,false) => CombineDeque (t1,t2.tailL,!first_active)
      case (true,false,_)      => CombineDeque (t1,t2.tailL,!first_active)

    }

    /* Q. Now implement addL for this new type of Deques: */

    def addL[B >: A] (b: B): Deque[B] = ???




    /* Ignore the RHS operations (they are symmetric). Just added below for
     * completeness. Do NOT fill in.
     */

    def headR: A = ???
    def tailR: Deque[A] = ???
    def addR[B >: A] (b: B): Deque[B] = ???




    /* Task 10. REASONING ABOUT PERSISTENT DATA STRUCTURE DESIGN
     *
     * The above implementation of deque uses one state variable (first_active)
     * to store which deque is active. First_active is switched approximately at
     * every operation. However this means that if we add and remove elements
     * from the combined deque in precise alternation, we will always be adding
     * to one deque, and always removing from the other (at least until it does
     * not deplete).  This is not a very interleaving strategy.  One way to fix
     * it, is to add another state variable to distinguish which of the combined
     * deques is active for adding, and which for removing separately.
     *
     * Q. Describe in English what changes would be required to the above
     * implementation of head and tail functions to accommodate this change. Do
     * not write the implementations in Scala. Indicative answer size is between
     * 50 and 200 words.
     *
     * Answer here: ...
     *
     *
     *
     *
     */

  }

}


