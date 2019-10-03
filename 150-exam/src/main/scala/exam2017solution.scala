// Advanced Programming 2017
// Example exam solutions.
// Andrzej Wąsowski
//
// Thanks to Adam and Ahmad for helping with debugging of questions and solutions.
//
// Note: many other solutions are often possible.  Also better solutions are
// possible sometimes, but not required for a top grade.

package adpro.exam2017solution

import fpinscala.monoids.Monoid
import fpinscala.monads.Functor
import fpinscala.state.State
import fpinscala.laziness.{Stream,Empty,Cons}
import fpinscala.laziness.Stream._
import fpinscala.parallelism.Par._
import scala.language.higherKinds
import adpro.data._
import adpro.data.FingerTree._
import monocle.Lens


object Q1 { // Association lists

  /**
   * Task 1: An association list is a List[(K,V)] storing pairs of values.  The
   * first element of each pair is a key (of type K), and the second element of
   * each pair is a value (of type V). Implement the function hasKey for
   * association lists. Given an association list l and a key k, it should
   * return true if and only if there exist an element with that key on the
   * list.
   */

  def hasKey[K,V] (l: List[(K,V)]) (k: K) :Boolean =
      l exists { case (k1,v) => k1 == k }

  /**
   * Task 2: Implement a reduceByKey operation for association lists, which uses
   * a reduction operator on values to create an association list, where all
   * keys are unique (and the values are obtained by reduction of elements from
   * the original list that had the same key).  Try to keep your solution
   * simple, don't optimize for efficiency.
   *
   * You may use the function hasKey from Task 1 (even if you failed to
   * implement it).
   */

  private def f[K,V] (ope: (V,V)=>V) (result: List[(K,V)], el: (K,V)) :List[(K,V)] =
    if (hasKey (result) (el._1))
        result map { case (k,v) => if (k==el._1) (k,ope (v,el._2)) else (k,v) }
    else el::result

  def reduceByKey[K,V] (l :List[(K,V)]) (ope: (V,V) => V) :List[(K,V)] =
      l.foldLeft[List[(K,V)]] (Nil) (f (ope))
  //
  // Note: I am using fold, not reduce, because the standard library has no
  // reduce for lists (apparently it has, but I discovered after solving, and it
  // is directional, see reduceLeft); It would be faster to sort the list first.

  // [below] An unanticipated solution by a student (using functions we have not used in
  // the course, and a bit of a cheat, but the solution was graded as correct).
  //
  // One weakness of the solution is that it uses a Map, and an association list
  // is usually implemented to provide simple maps (so it should be possible,
  // and natural to do it without translating to other data structures).
  // The student got the full points though.

  def reduceByKeyViaGroup[K,V] (l: List[(K,V)]) (ope: (V,V)=>V) :List[(K,V)] =
    (l groupBy (_._1) map { case (k,v) => (k,v map {_._2} reduce ope) }).toList

  // Can be slightly simplified using Map.mapValues but this is a method we have
  // not used in the course.

  /**
   * Task 3.  Consider the following function transforming an association list
   * of identifiers paired with tokenized sentences, into list of identifiers
   * paired with single tokens (a problem extracted from our sentiment analysis
   * project).  Rewrite this function using for-comprehension instead of map and
   * flatMap.
   **/

  def separate (l :List[(Int,List[String])]) :List[(Int,String)] =
    l flatMap { idws => idws._2 map { w => (idws._1,w) } }

  def separateViaFor (l :List[(Int,List[String])]) :List[(Int,String)] =
    for {
      idws <- l
      w    <- idws._2
    } yield (idws._1,w)

}


object Q2 {

  /**
   * Task 4. Consider the following binary tree type that embeds lists of
   * elements of type A, instead of simple elements in the internal nodes.     */

  trait TreeOfLists[+A]
  case object LeafOfLists  extends TreeOfLists[Nothing]
  case class BranchOfLists[+A] (
    data: List[A],
    left: TreeOfLists[A],
    right: TreeOfLists[A]
  ) extends TreeOfLists[A]

  /* Generalize these types to use \emph{any} generic collection C[_] instead of
   * List[_]
   **/

  trait TreeOfCollections[C[+_],+A]
  case class LeafOfCollections[C[+_]] () extends TreeOfCollections[C,Nothing]
  case class BranchOfCollections[C[+_],+A] (
    data: C[A],
    left: TreeOfCollections[C,A],
    right: TreeOfCollections[C,A]
  ) extends TreeOfCollections[C,A]

  /**
   * Task 5.
   * For the TreeOfLists we use a special map function,  that takes a
   * transformer function for elements of type A and applies it to all As in all
   * lists in all nodes of a tree:
   */

    def map[A,B] (t: TreeOfLists[A]) (f: A => B) :TreeOfLists[B] = t match {
      case LeafOfLists => LeafOfLists
      case BranchOfLists (data,left,right) =>
          BranchOfLists (data map f, map (left) (f), map (right) (f))
    }

  /* We want to generalize this function from Lists to any Collection C[_], so
   * to use TreeOfCollections instead of TreeOfLists.  This requires assuming
   * that an instance of Functor[C] exists (textbook Chapter 11).  Write the
   * generalized map function.
   */
  def map[A,C[+_],B] (t: TreeOfCollections[C,A]) (f: A => B) (implicit functor: Functor[C]): TreeOfCollections[C,B] =
    t match {
      case LeafOfCollections() => LeafOfCollections[C] ()
      case BranchOfCollections (data,left,right) =>
        BranchOfCollections(functor.map[A,B] (data) (f), map (left) (f), map (right) (f) )
    }

  // Note: The use of implicit here is not essential
}



object Q3 {

  /**
   * Task 6. Consider the following two functions:
   */

  def p (n: Int): Int = { println (n.toString); n }

  def f (a: Int, b: Int): Int = if (a > 10) a else b

  /* Assume that the argument of p is call-by-value; We will be varying the
   * argument passing semantics of f.
   *
   * What is printed by the following expression?
   */

  p ( f( p(42), p(7) ) )

  /* A. Assuming that f is call-by-value in both arguments.
   * B. Assuming that f is call-by-name in both arguments.
   * C. Assuming that f follows the lazy evaluation strategy for both arguments
   * (note: not available directly in Scala).
   */

  // assuming f is call-by-value.
  // 42
  // 7
  // 42

  // assume f is call-by-name
  // 42
  // 42
  // 42

  // assume that f follows the lazy evaluation convention for passing arguments
  // (not directly available in Scala)
  //
  // 42
  // 42

}



object Q4 {

/**
 * Task 7. We consider a simple finite state automaton that models a coffee
 * maker. The machine has two inputs: you can insert a Coin, and you can press
 * Brew. The machine has a counter estimating the amount of coffee bean portions
 * available in storage, and another integer value counting the accumlated
 * coins.  We model the inputs using the following types:
 */

 sealed trait Input
 case object Coin extends Input
 case object Brew extends Input

 /* The machine has two discrete states:  a ready state (accepting coins only)
  * and a busy state (aka not ready), in which one can press the Brew button to
  * make coffe.  The following type captures the entire state of the automaton:
  */

 case class MachineState (ready: Boolean, coffee: Int, coins: Int)

 /* The transition rules of the automaton are:
  *
  * - Ignore all the input if it contains no coffee beans (no state change)
  * - Inserting a coin into a machine causes it to become busy (if there are
  *   still coffee bean portions left). It also increases the number of coins
  *   accumulated.
  * - Pressing Brew on a busy machine will cause it to deliver a cup of coffee
  *   (which changes the state by taking one coffee portion away).
  * - Pressing Brew on a machine which is not busy (a ready machine) has no
  *   effect
  * - Inserting two or more coins in arrow is possible, but you will only get
  *   one coffee anyway.  The machine is a bit simplistic: the new coin is just
  *   gladly consumed.
  *
  * Implement a function step that given an input and a state computes a
  * new state, as per the above rules:
  */

  // Give types here (also needed for the next task)
  def step (i: Input) (s: MachineState) :MachineState =  (s,i) match {
    case (MachineState(_,0,_),_) => s
    case (MachineState(_,coffee,coins), Coin) =>
      MachineState (false,coffee,coins+1)
    case (MachineState(false,coffee,coins), Brew) =>
      MachineState (true,coffee-1,coins)
    case _ => s
  }

  /*
   * Task 8. The method simulateMachine (type below) should execute the machine
   * based on the list of inputs and return the number of coffee bean portions
   * and coins accumulated  at the end. For example, if initially the machine
   * has 42 coffe portions and 10 coins, and a total of 4 coffees are
   * successfully sold for one coin each, then the output should be (38, 14).
   *
   * Use the State monad type from the course to implement this function.  You
   * shall use the function step from the previous task (even if you have not
   * implemented it).
   */

  def simulateMachine (initial: MachineState) (inputs: List[Input]) :(Int,Int) =  {

    val ms :List[State[MachineState,Unit]] =  inputs map (i => State.modify (step (i)))
    val m :State[MachineState,Unit] = State sequence (ms) map (_ => Unit)
    val MachineState(ready,coffee,coins) = (m run initial)._2
    (coffee,coins)
  }

  def simulateMachineViaFor (initial: MachineState) (inputs: List[Input]) :(Int,Int) =  {

    val m: State[MachineState,Unit] = for {
      _ <- State.sequence { inputs map (i => State modify (step (i))) }
    } yield ()

    val MachineState(ready,coffee,coins) = (m run initial)._2
    (coffee,coins)
  }

  // A nice solution by one of the examinees:

  def simulateMachineViaFor2 (initial: MachineState) (inputs: List[Input]): (Int,Int) = {
    val stepper = for {
      _ <- State.sequence(inputs.map(x => State.modify(step(x))))
      s <- State.get
    } yield (s.coffee, s.coins)
    stepper.run(initial)._1
  }


  def simulateMachine1 (initial: MachineState) (inputs: List[Input]): (Int, Int) = (for {
    _ <- State.sequence(inputs.map(State.modify[MachineState] _ compose step))
    s <- State.get
  } yield(s.coffee, s.coins)).run(initial)._1

}


object Q5 {

  /* Task 9. Imagine that we want to model as a lazy stream a data access
   * activity that is relatively slow.  With this data acess it is expensive to
   * force one element (say downloading from a remote location) but it is
   * relatively faster to take elements in batches of k (k is a small natural
   * number), as the cost of transmission is dominated by the cost of
   * establishing the connection.
   *
   * To treat such data effciently in a lazy stream we can model this as a
   * stream of type Stream[List[A]], where each list in the stream has k
   * elements fetched.  Now forcing one element of the string will load (buffer)
   * the entire list of k elements.  However, it is inconvenient to program with
   * streams of lists, if the rest of the program logic needs to process the
   * elements of type A one-by-one.
   *
   * Implement a function flatten that converts a Stream[List[A]] to a Stream[A]
   * in the obious way (as if the lists where 'concatenated into a single
   * stream').  Do so carefully, to avoid forcing the next list in the stream if
   * not necessary.  It is fine to force the head of the stream always (as often
   * done earlier in the course), but do not force deeper elements.
   */

  // Best solution (but assumes we have List.toStream, which is a bit unclear
  // in the context):

  // def flatten[A] (s: =>Stream[List[A]]) :Stream[A] = s flatMap (_.toStream)

  // The above solution is commented out to avoid compilation errors. They
  // appear because we are using fpinscala.Streams in this file, but standard
  // Library immutable lists (obviously standard library does not provide
  // toStream for our fpinscala Streams).
  //
  // A similar trick, but using a convenience constructor:
  //
  // def flatten1 [A] (s: =>Stream[List[A]]) :Stream[A] = s flatMap[A] (l => Stream(l: _*))


  // A good solution (for 10) that does not  make this assumption (would also be
  // graded top as the exercise was unclear whether this is a valid assumption
  // or not)

  def flatten[A] (s: =>Stream[List[A]]) :Stream[A] = s match {
    case Empty => empty
    case Cons(h,t) => h() match {
      case Nil => flatten (t())
      case (lh::lt) => cons (lh, flatten (Cons(()=>lt,t)) )
    }
  }

  // Another good solution (12), in between the first and the last can be
  // achieved by combinign flatMap and List.foldRight


}


object Q6 {

  /* Task 10.  Recall the function exists from the standard list interface:
   *
   * def exists[A] (l: List[A]) (p: A =>Boolean): Boolean
   *
   * The function checks whether at least a single element of a list l satisfies
   * the predicate satisfy the predicate p.
   *
   * Implement a simple parallel version of exists that would work on short
   * lists, of sizes smaller than the  number of available threads. This new
   * function, parExists, should check in parallel whether each and single
   * element of the list l satisfies the predicate p, and then should combine
   * the results so that the outcome is equivalent to the sequential exists.
   *
   * The resulting type should be Par[Boolean] (so a computation that returns
   * Boolean, if scheduled on a parallel computing resource; as defined in the
   * course).
   */

  def parExists[A] (as: List[A]) (p: A => Boolean): Par[Boolean] = {
    val bs :Par[List[Boolean]] = parMap (as) (p)
    map[List[Boolean],Boolean] (bs) (bs => bs exists (x => x) )
  }

}


object Q7 {

  /**
   * Task 11. (For MSc and Master students only)
   *
   * The Hinze/Paterson paper on FingerTrees introduces a logarithmic time
   * concatenation operation.  A simpler concatanation method for deques can be
   * implemented in linear time (in the size of the deque), by iteratively
   * adding all the elements of the second deque to the right of the first
   * deque.  Implement this operation.
   *
   * If you need, you can rely on other elements of the FingerTree
   * implementation than quoted here.
   */


  //  def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, t: FingerTree[A]) :Z = ??? // assume that this is implemented
  //  def reduceR[A,Z] (opr: (A,Z) => Z) (t: FingerTree[A], z: Z) :Z = ??? // assume that this is implemented

  //  trait FingerTree[+A] {
  //  def addL[B >:A] (b: B) :FingerTree[B] = ??? // assume that this is implemented as in the paper
  //  def addR[B >:A] (b: B) :FingerTree[B] = ??? // assume that this is implemented as in the paper
  // }

  def concatenate[A, B >: A] (left: FingerTree[A]) (right: FingerTree[B]) :FingerTree[B] =
      reduceL[B,FingerTree[B]] { case (t,b) => t addR b } (left, right)

}


object Q8 {


  /* Task 12 (MSc students only). Implement a lense nullOption between T and
   * Option[T].  This lense should translate a value a of type T into Some(c)
   * and a null value into None.
   */

  def nullOption[T] = Lens[T,Option[T]] (
    c => if (c == null) None else Some(c)) (
    a => _ => a getOrElse (null.asInstanceOf[T]))


  /* Then respond to the three following questions:
   *
   * 1. Does this lense satisfy the put-get low, and why?
   *
   * 2. Does this lense satisfy the get-put low, and why?
   *
   * 3. Does this lense satisfy the put-put low, and why?
   */

  // the answer should be positive to all of these. For the first question
  // though there is a twist if we admit Some(null) as a value of Option[T].
  // Then  put of Some(null) gives null, and get of null gives None.  So
  // technically this lense is not well behaved.  In practice it is, as we
  // usually do not admit null value as a parameter for Some, yet the type
  // system does not enforce this.

}
