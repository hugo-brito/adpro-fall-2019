// Advanced Programming 2016
// Example exam solutions.
// Andrzej Wąsowski
//
// Thanks to Jonas Lonholdt, Mads Rosendahl, Ahmad Al-Sibahi for helping with
// debugging of questions and solutions.
//
// Note: many other solutions are often possible.  Also better solutions are
// possible sometimes, but not required for a top grade.

package adpro.exam2016solution

object Q1 {

  /**
   * Task 1: The following function computes a 16 bit checksum of a character
   * stream. Translate it to a referentially transparent version.
   */

  def checksumImp (in: String) :Int = {
    var result = 0
    for (c <- in.toList)
      result = (result + c.toInt) % 0xffff
    return result
  }

  // SOLUTION

  def checksumFun (in: String) :Int = {

    def ck (in :List[Char]) (sum: Int) :Int = in match {
      case Nil => sum
      case h::t => ck (t) ((h.toInt + sum) % 0xffff) // << TL
    }
    ck (in.toList) (0)
  }

  // TASK 2: explain whether your solution is tail recursive and why (how can we
  // see whether it is tail recursive or not).
  //
  // The above solution happens to be tail recursive.  The tail recursive call
  // is marked with a "<< TL" comment on the right end of the line.  The call is
  // tail recursive because it is in the, so called, tail position (so the value
  // returned by ck in the above line, is also directly the value of the calling
  // function)
  //
  // Note: There are solutions not using recursion (for example delegating to
  // foldLeft). In such case it is OK to say that the function is not recursive
  // at all, so it is not tail recursive.

}


object Q2 {

  import fpinscala.monads.Functor
  import scala.language.higherKinds

  // Task 3: Implement a function onList that promotes any A => A function to
  // operate on lists of As pointwise.  For instance, if "f(c)" capitalizes a
  // character c, then (onList (f)) capitalizes an entire list of characters.

  def onList[A] (f: A => A) : List[A] => List[A] = _.map (f)

  // Task 4: Now assume any collection constructor C[_] for which we have a
  // type class inscance Functor[C]  as seen in the course.  Generalize onList
  // to a function onCollection that provides the same functionality for any
  // collection:

  def onCollection[C[_],A] (f: A => A) (implicit functorC: Functor[C]) : C[A] => C[A] =
    c => functorC.map (c) (f)
}


object Q3 {

  import fpinscala.monoids.Monoid
  import scala.language.higherKinds

  /**
   * Task 5: Implement a function foldBack that folds an operator of a
   * monoid M (for which an instance Monoid[M] exists, as defined in the book),
   * by traversing through the list twice. If the operator is "+" and the List
   * is : List(x1,x2,x3) it should compute:
   * (z + (x1 + (x2 +(x3 + (x3 + (x2 + (x1 + z)))))))
   *
   * where z is the zero of the monoid M.
   */

  // SOLUTION

  def foldBack[A] (l: List[A]) (implicit M: Monoid[A]) :A =
    (l ++ l.reverse).foldRight (M.zero) (M.op)

  // The selection of the fold is immaterial, due to associativity of the
  // monoid.  The reverse is important.

}



object Q4 {

  /**
   * T6
   * Let the following type represent a list of computations that may fail.
   * The result of the computation is either a value of type A or an error
   * message.
   */

  type Computation[A] = A => Either[A,String]

  /* Implement a function 'run' that given a list of computations and an initial
   * value of type A, executes the computations from the head to tail.
   *
   * If any computation fails, then 'run' tries the next one on the list, but it
   * collects the error messages.
   */

  def run[A] (init: A) (progs: List[Computation[A]]) :(A,List[String]) = {

    def step[A] (state: (A,List[String]), c: Computation[A]) :(A,List[String]) =
      c (state._1) match {
        case Left (a) => (a,state._2)
        case Right(err) => (state._1,err::state._2)
      }

    val (a,errs) = progs.foldLeft (init,List[String]()) (step)
    (a,errs.reverse)
  }

}


object Q5 {

  /**
   * Task 7. Consider a type of lazy binary trees:
   */

  sealed trait Tree[A]
  case class Branch[A] (l: () => Tree[A], a: A, r:() => Tree[A]) extends Tree[A]
  case class Leaf[A] (a: A) extends Tree[A]

  /**
   * Implement a function that computes the mutliplication of values in
   * a Tree[Int] (note that the function should stop exploring the possibly
   * infinite tree as  soon as the result of the multiplication is zero).
   */

  // SOLUTION
  def mult (t: Tree[Int]) :Int = t match {
    case Leaf (a) => a
    case Branch (l,a,r) =>
      if (a==0) 0
      else {
        val ml = mult(l())
        if (ml!=0) ml*a*mult(r())
        else 0
      }

  }
  // The above solution would still fail if some left subtree is infinite, while
  // the right subtree would contain a zero.  A breadth-first-search is an
  // even more lazy solution, with more termination guarantee, but the exercise
  // is not specific about it.
  //
  // When grading we requires that the function is lazy at *least* in one of the
  // subtrees. So if it terminated as soon as the result of the left subtree was
  // zero, then it was alright.  However, it was not alright, if the function
  // always explored both subtrees, even if one of them turned out to be zero.

  /**
   * Task 8. Describe in  English [or Danish] how would  you test this
   * function. In  particular, explain  how  can you  ensure that  the
   * function is  not overly  eager, exploring too  large part  of the
   * tree.
   *
   * Place your answer below.
   */

    /* I would construct infinite trees with zero and non-zero values on top,
     * and then write a generator that constructs a finite tree prefix, with
     * either a Leaf node at the bottom, or one of my infinite trees.  (for
     * simplicity zero should not be used anywhere in the tree, besides the
     * root).
     *
     * The zero-trees would have to  have a failing node (for instance exception
     * thrown) just  under the  zero node,  to ensure  that whenever a
     * computation enforces them, should crash.
     *
     * All the zero-only and all the leaf-only completed trees should terminate
     * with zero (resp. finite) amount. The other tests would not terminate, I
     * would timeout them.
     *
     * I would run unit tests or property tests on all these cases:
     * - finite tree
     * - "infinite" tree with zero node
     * - "infinite" tree without zeros
     *
     * I could also make an unfolding random infinite tree; this test should
     * also terminate probabilistically.  The chance of getting a tree without
     * any zero node is, well, zero.
     *
     * For terminating tests we want to establish whether the function produces
     * a correct value.  This can be achieved by either using fixed test cases
     * with known multiplication result, or a generator that takes the node
     * values from a given random list of numbers.  In the latter case  a simple
     * oracle function can be computed by folding the multiplication monoid over
     * the list.
     *
     * I would decide that the testing is sufficient by checking whether I have
     * a decent branch coverage.  Otherwise, I would add unit tests to reach
     * untested lines.
     *
     * COMMENTARY: Important components of a perfect answer: naming the test
     * method (property, unit, scenario ... -- you choose), test selection
     * criteria, stating sufficiency condition for testing, addressing eagerness
     * testing, specifying correctness oracle (not only for the zero case, but
     * also for the non-zero case).
     */

}


object Q6 {

  /**
   * Task  9. Consider the  following types  for representing  Peano's
   * definition of natural numbers.
   */

  sealed trait Nat[+A]
  case object Zero extends Nat[Unit]
  case class Succ[A] (pred: A) extends Nat[A]

  /* the first three natural numbers are represented as follows: */

  val zero = Zero
  val one = Succ (zero)
  val two = Succ (one)

  /* This is not a practical representation (in fact it is an entirely
   *useless one). We  use it  to construct  a simple  exercise.  Write
   *down type  annotations for  the above values  (zero, one,  two) in
   *terms of  the Nat type  constructor (what  are the types  of zero,
   *one, two using the Nat constructor).
   */

  object SOLUTION11 {

    val zero :Nat[Unit]           = Zero
    val one  :Nat[Nat[Unit]]      = Succ (zero)
    val two  :Nat[Nat[Nat[Unit]]] = Succ (one)

  }

  /**
   * Task 10. Write  function plus2 that  takes a representation  of a
   * annatural number, d returns a representation of the number bigger
   * anby  two. Make  the type  notations  in  the function  signature
   * anexplicit
   */

  object SOLUTION12 {

    def plus2[A] (x: A) :Nat[Nat[A]] = Succ(Succ(x))

    // or

    def plus2[A] (x: Nat[A]) :Nat[Nat[Nat[A]]] = Succ(Succ(x))

  }

}
