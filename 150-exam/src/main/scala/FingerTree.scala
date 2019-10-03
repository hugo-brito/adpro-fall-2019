package adpro
import scala.language.higherKinds

// This implementation is designed to be eager, following the regular strictness
// of Scala.  However it would be an interesting exercise to extend it so that
// it is possibly lazy, like in the paper of Hinze and Paterson.  The obvious
// choice is to make values of elements stored in the queue lazy.  Then there is
// also a discussion of possible suspension of the middle element of the tree on
// page 7.


// Simulating a package with an object, because type declarations
// can only be placed in objects.
//
// We changed the type of reduceRs to not use curried operators, but regular
// binary operators.  This is more natural in Scala, and gives easier to read
// syntax of expressions.
//
// TODO: understand why the container is called digit.

// we did the same with addL and addR

object data {

  // The interface spec for reducible structures, plus two useful derived
  // reducers that the paper introduces (toList and toTree)

  trait Reduce[F[_]] {
    def reduceR[A,B] (opr: (A,B) => B) (fa: F[A], b: B) :B
    def reduceL[A,B] (opl: (B,A) => B) (b: B, fa: F[A]) :B

    def toList[A] (fa: F[A]) :List[A] = reduceR[A,List[A]] (_::_) (fa,Nil)
    def toTree[A] (fa :F[A]) :FingerTree[A] =
      reduceR[A,FingerTree[A]] (FingerTree.addL) (fa, Empty())
  }

  // Types for Finger trees after Hinze

  type Digit[A] = List[A]

  sealed trait Node[+A] { def toList :List[A] = Node.toList (this) }
  case class Node2[A] (l :A, r :A) extends Node[A]
  case class Node3[A] (l: A, m: A, r: A) extends Node[A]

  trait Deque[+A] {

    def addL[B >:A] (b: B) :Deque[B]
    def addR[B >:A] (b: B) :Deque[B]

    def empty: Boolean
    def nonEmpty: Boolean

    def headL :A
    def tailL :Deque[A]
    def headR :A
    def tailR :Deque[A]

  }

  sealed trait FingerTree[+A] extends Deque[A] {
    def addL[B >:A] (b: B) :FingerTree[B] = FingerTree.addL (b,this)
    def addR[B >:A] (b: B) :FingerTree[B] = FingerTree.addR (this,b)
    def toList :List[A] = FingerTree.toList (this)

    def empty = false // added for convenience
    def nonEmpty = true

    def headL :A = FingerTree.headL (this)
    def tailL :FingerTree[A] = FingerTree.tailL (this)
    def headR :A = FingerTree.headR (this)
    def tailR :FingerTree[A] = FingerTree.tailR (this)
  }

  case class Empty () extends FingerTree[Nothing] {
    override def empty = true
    override def nonEmpty = false
  }
  case class Single[A] (data: A) extends FingerTree[A]
  // pr - prefix, m - middle, sf - suffix
  case class Deep[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]) extends FingerTree[A]

  // Types of views on trees (perhaps we can kill this using extractors)
  // The types are provided for educational purposes.  I do not use the view
  // types in my implementation. I implement views as Scala extractors.

  // In the paper views are generic in the type of tree used. Here I make them
  // fixed for FingerTrees.

  //  sealed trait ViewL[+A]
  //  case class NilTree () extends ViewL[Nothing]
  //  case class ConsL[A] (hd: A, tl: FingerTree[A]) extends ViewL[A]

  // Left extractors for Finger Trees (we use the same algorithm as viewL in the
  // paper)

  object NilTree {
    def unapply[A] (t: FingerTree[A]) :Boolean = t match {
      case Empty () => true
      case _ => false
    }
  }

  object ConsL {
    def unapply[A] (t: FingerTree[A]) :Option[(A,FingerTree[A])] = t match {
      case Empty () => None
      case Single (x) => Some (x, Empty())
      case Deep (pr,m,sf) => Some (pr.head, FingerTree.deepL (pr.tail, m, sf))
    }
  }

  // Exercise (right extractors) (we reuse NilTree, no need for a new one)

  object ConsR {
    def unapply[A] (t: FingerTree[A]) :Option[(FingerTree[A],A)] = t match {
      case Empty () => None
      case Single (x) => Some (Empty(), x)
      case Deep (pr, m, sf) =>
        val sfr = sf.reverse
        Some (FingerTree.deepR[A] (pr,m,sfr.tail), sfr.head)
    }
  }

  // several convenience operations for Digits.
  //
  object Digit extends Reduce[Digit] {

    def reduceR[A,Z] (opr: (A,Z) => Z) (d: Digit[A], z: Z) :Z = d.foldRight (z) (opr)
    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, d: Digit[A]) :Z = d.foldLeft  (z) (opl)

    // Digit inherits toTree from Reduce[Digit] that we will also apply to other
    // lists, but this object is a convenient place to put it (even if not all
    // lists are digits)

    // This is a factory method that allows us to use Digit (...) like a
    // constructor
    def apply[A] (as: A*) : Digit[A] = List(as:_*)

    // This is an example of extractor, so that we can use Digit(...) in pattern
    // matching.  Case classes have extractors automatically, but Digit defined
    // as above is not a case class, but just a type name.
    def unapplySeq[A] (d: Digit[A]): Option[Seq[A]] = Some (d)
  }


  object Node extends Reduce[Node] {

    def reduceR[A,Z] (opr: (A,Z) => Z) (n :Node[A], z: Z) :Z = n match {
      case Node2 (l,r) =>  opr (l, opr (r, z))
      case Node3 (l,m,r) => opr (l, opr (m, opr (r, z)))
    }

    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, n :Node[A]) :Z = n match {
      case Node2 (l,r) => opl (opl (z, l), r)
      case Node3 (l,m,r) => opl (opl (opl (z, l), m), r)
    }

  }



  // all functions are defined like in the JFP paper (not methods).  The methods
  // are created just for delegation.  This should make it easier to understand.
  //
  // making the object implicit makes FingerTrees accessible to methods that
  // require an instance of Reduce type class (for instance toTree)

  object FingerTree extends Reduce[FingerTree] {

    def reduceR[A,Z] (opr: (A,Z) => Z) (t: FingerTree[A], z: Z) :Z = t match {
      case Empty () => z
      case Single (x) => opr (x,z)
      case Deep (pr, m, sf) =>
        val o1 = Digit.reduceR (opr) _
        val o2 = FingerTree.reduceR (Node.reduceR (opr) _) _
        o1 (pr, o2 (m, o1 (sf,z)))
    }

    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, t: FingerTree[A]) :Z = t match {
      case Empty () => z
      case Single (x) => opl (z,x)
      case Deep (pr, m, sf) =>
        val o1 = Digit.reduceL (opl) _
        val o2 = FingerTree.reduceL (Node.reduceL (opl) _) _
        o1 (o2 (o1 (z, pr), m), sf)
    }

    def addL[A] (a: A, t: FingerTree[A]) :FingerTree[A] = t match {
      case Empty () => Single (a)
      case Single (b) => Deep (Digit(a), Empty(), Digit(b))
      case Deep (Digit (b,c,d,e), m, sf) =>
        Deep (Digit(a,b), addL[Node[A]] (Node3 (c,d,e), m), sf)
      case Deep (pr, m, sf) => Deep (a::pr, m, sf)
    }

    def addR[A] (t: FingerTree[A], a: A) :FingerTree[A] = t match {
      case Empty() => Single (a)
      case Single(b) => Deep (Digit(b), Empty(), Digit(a))
      case Deep (pr, m, Digit(e,d,c,b)) =>
        Deep (pr, addR[Node[A]] (m, Node3 (e,d,c)), Digit(b,a))
      case Deep (pr, m, sf) => Deep (pr, m, sf ++ List(a))
      // It would likely be more efficient (constant factor)
      // to do a::sf instead and reinterpret the invariant for sf representation
      // (the last element is the head of the list). I have not done that to
      // keep the implementation closer to the paper.
    }


    // This is a direct translation of view to Scala. It is commented out, as we
    // use extractors instead, see above objects NilTree and ConsL (this is an
    // alternative formulation which is more idiomatic Scala, and slightly
    // better integrated into the language than the Haskell version).
    // In Haskell we need to call viewL(t) to pattern match on views.  In Scala,
    // with extractors in place, we can directly pattern match on t.
    //
    // def viewL[A] (t: FingerTree[A]) :ViewL[A] = t match {
    //   case Empty() => NilTree()
    //   case Single (x) => ConsL (x, Empty())
    //   case Deep (pr,m,sf) => ConsL (pr.head, deepL (pr.tail, m, sf))
    // }

    // A smart constructor that allows pr to be empty
    def deepL[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]) :FingerTree[A] =
      pr match {
        case Nil => m match { // don't have to call viewL here bcs we use extractors
          case NilTree () => Digit.toTree (sf)
          case ConsL (a,m1) => Deep (a.toList, m1, sf)
        }
        case pr => Deep (pr, m, sf)
      }

    def headL[A] (t: FingerTree[A]) :A = t match { case ConsL(h,_) => h }
    def tailL[A] (t: FingerTree[A]) :FingerTree[A] = t match { case ConsL(_,t) => t }
    def headR[A] (t: FingerTree[A]) :A = t match { case ConsR(_,h) => h }
    def tailR[A] (t: FingerTree[A]) :FingerTree[A] = t match { case ConsR(t,_) => t }

    // Exercise: implement deepR and ConsR
    def deepR[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]) :FingerTree[A] =
      sf match {
        case Nil => m match { case NilTree ()   => Digit.toTree (pr)
                              case ConsR (m1,a) => Deep (pr, m1, a.toList) }
        case sf => Deep (pr, m, sf)
      }

  }

}

