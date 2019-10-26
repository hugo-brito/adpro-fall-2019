package adpro.exam2017solution
import scala.language.higherKinds

import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import monocle.Lens
import fpinscala.monads.Functor

class  exam2017Spec extends FreeSpec with Checkers {

  "QUESTION 1" - {

    import adpro.exam2017solution.Q1._

    "for any list containing an element hasKey returns true " in check {

      def listWith[A](el: A) (implicit arb: Arbitrary[List[A]]) :Gen[List[A]] =
        arb.arbitrary flatMap { l => arb.arbitrary map { k => k ::: el::l } }

      forAll { (n: Int) =>
        forAll(listWith((n,42.0+n))) { (l: List[(Int,Double)]) => hasKey (l) (n) }
      }

    }

    "for any list not containing an element hasKey returns false" in check {

      def listWithout[K,V] (k: K) (implicit arb: Arbitrary[List[(K,V)]]) :Gen[List[(K,V)]] =
        arb.arbitrary map { l => l filter { _._1 != k } }

      forAll { (n: Int) =>
        forAll(listWithout[Int,Double](n)) { (l: List[(Int,Double)]) => !hasKey (l) (n) }
      }

    }

    "simple reduceByKey sccenario test" in {
      val l = List ((1,1),(2,42),(1,3),(2,58),(3,7)).reverse
      reduceByKey[Int,Int] (l) { _ + _ } shouldBe List ((1,4), (2,100), (3,7))
    }

    "for comprehension equivalent to map and flatMap" in {
      forAll { (l :List[(Int,List[String])]) => separate (l) == separateViaFor (l) }
    }

  }

  "QUESTION 2" - {

    import Q2._

    "TreeOfLists T5+T6" - {

      "map works well on empty tree" in check {
        val t: TreeOfLists[Int] = LeafOfLists
        forAll { (f: Int=>Char) => map (t) (f) == t }
      }

      def genTreeOfLists[A] (a: List[A], depth:Int) :Gen[TreeOfLists[A]] = depth match {
        case 0 => Gen.const (LeafOfLists)
        case n =>
          for {
            m <- Gen.choose (0,n-1)
            left <- genTreeOfLists (a, m)
            right <- genTreeOfLists (a, m)
          } yield (BranchOfLists(a,left,right))
      }

      "map works well on empty lists" in check {
        forAll (genTreeOfLists(List[Int](),100)) { t =>
          forAll { (f: Int=>Char) =>
            map (t) (f) == t
          }
        }
      }

      "map works well on fixed tree" in {

        val t1 = BranchOfLists( List(42),
          BranchOfLists(43::Nil,LeafOfLists,LeafOfLists),
          BranchOfLists(44::Nil,LeafOfLists,LeafOfLists) )
        val t2 = BranchOfLists( List(43),
          BranchOfLists(44::Nil,LeafOfLists,LeafOfLists),
          BranchOfLists(45::Nil,LeafOfLists,LeafOfLists) )

        map (t1) (x => x+1) shouldBe t2

      }

    }

    "Tree Of Collections" - {

      "map works well on empty collection" in check {
        val t: TreeOfCollections[List,Int] = LeafOfCollections[List]
        forAll { (f: Int=>Char) => map (t) (f) (Functor.listFunctor) == t }
      }

      def genTreeOfCollections[C[+_],A] (a: C[A], depth:Int) :Gen[TreeOfCollections[C,A]] = depth match {
        case 0 => Gen.const (LeafOfCollections[C]())
        case n =>
          for {
            m <- Gen.choose (0,n-1)
            left <- genTreeOfCollections (a, m)
            right <- genTreeOfCollections (a, m)
          } yield (BranchOfCollections(a,left,right))
      }

      "map works well on empty collections" in check {
        forAll (genTreeOfCollections(List[Int](),100)) { t =>
          forAll { (f: Int=>Char) =>
            map (t) (f) (Functor.listFunctor) == t
          }
        }
      }

      "map works well on fixed tree" in {

        val t1 = BranchOfCollections[List,Int]( List(42),
          BranchOfCollections(43::Nil,LeafOfCollections[List](),LeafOfCollections[List]()),
          BranchOfCollections(44::Nil,LeafOfCollections[List](),LeafOfCollections[List]()) )
        val t2 = BranchOfCollections[List,Int]( List(43),
          BranchOfCollections(44::Nil,LeafOfCollections[List](),LeafOfCollections[List]()),
          BranchOfCollections(45::Nil,LeafOfCollections[List](),LeafOfCollections[List]()) )

        map (t1) (x => x+1) (Functor.listFunctor) shouldBe t2

      }



    }

  }

  "QUESTION 3 has no tests" - { }

  "QUESTION 4" - {

    import Q4._

    import Gen._

    val genM: Gen[MachineState] = for {
      ready  <- oneOf  (true,false)
      coffee <- choose (0,100)
      coins  <- choose (0,100)
    } yield (MachineState(ready,coffee,coins))

    val EmptyMachine        = genM retryUntil (_.coffee == 0)
    val NonEmptyMachine     = genM retryUntil (_.coffee > 0)
    val ReadyMachine        = genM retryUntil (_.ready)
    val BusyNonEmptyMachine = genM retryUntil (m => !m.ready && m.coffee > 0)

    "TASK 7. Transition step" - {

      "empty machine" in check { forAll (EmptyMachine) { m => step (Coin) (m) == m } }

      "inserting a coin into a machine causes it to become busy" in check {
        forAll (NonEmptyMachine) { ms => ! ((step (Coin) (ms)).ready) } }

      "it also increases the number of accumulated coins" in check {
        forAll (NonEmptyMachine) { ms => step (Coin) (ms).coins == ms.coins + 1} }

      "pressing Brew on a busy machine uses one coffee portion" in check {
        forAll (BusyNonEmptyMachine) { ms => step (Brew) (ms).coffee == ms.coffee - 1} }

      "pressing Brew on a busy machine moves to ready" in check {
        forAll (BusyNonEmptyMachine) { ms => step (Brew) (ms).ready } }

      "pressing Brew on a ready machine has no effect" in check {
        forAll (ReadyMachine) { ms => step (Brew) (ms) == ms } }
    }

    "TASK 8. Simulation" - {

      implicit val arbInput = Arbitrary[Input] (oneOf(Coin,Brew))

      "the two solutions are equivalent" in check {
        forAll (genM) { ini =>
          forAll { (inputs: List[Input]) =>
            simulateMachine (ini) (inputs) == simulateMachineViaFor (ini) (inputs) }
        }
      }

      val trace = List(Coin,Coin,Brew,Coin,Brew,Brew)
      val gen = genM retryUntil (_.coffee > 2)
      "coin coin brew coin brew brew" in check {
        forAll (gen) { ini => simulateMachine (ini) (trace)._2 == ini.coins + 3 } &&
        forAll (gen) { ini => simulateMachine (ini) (trace)._1 == ini.coffee - 2 }
      }

    }

  }



  "QUESTION 5. Task 9" - {

    import fpinscala.laziness.Stream._
    import fpinscala.laziness._
    import Q5._

    "simple scenario" in {
      flatten (Stream(List(1,2),List(),List(3,4,5,6),List(7),List(0))).toList shouldBe List(1,2,3,4,5,6,7,0)
    }

    "empty" in {  flatten(Stream()).toList shouldBe Nil }

    "laziness test" in {
      flatten (cons(List(1,2),???)).headOption shouldBe  Some(1)
    }


  }

  "QUESTION 6 has no tests" - { }

  "QUESTION 7" - {

    import adpro.data._


    val t = List(1,2,3).foldLeft[FingerTree[Int]] (adpro.data.Empty()){ case (t,b) => t addR b }

    "concatenate two triplets well (via toList)" in {
      Q7.concatenate (t) (t).toList shouldBe List(1,2,3,1,2,3)
      Q7.concatenate (t) (t).toList should not equal List(1,2,3,3,2,1)

    }


  }


  "QUESTION 8" - {

    import Q8._

    "nullOption get on null" in {
        nullOption[Integer].get (null.asInstanceOf[Integer]) shouldBe None }

    "nullOption set on None" in {
        nullOption[Integer].set (None) (42) shouldBe null.asInstanceOf[Integer] }

    "nullOption get on int" in check {
      forAll { (n:Int) => nullOption[Integer].get (n) == Some(n) }
    }

    "nullOption set on int" in check {
      forAll { (n: Int) =>
        forAll { (m: Int) =>
          nullOption[Integer].set (Some(n)) (m) == n
        }
      }
    }

    def PutGet[C,A] (l: Lens[C,A])
      (implicit aA: Arbitrary[A], aC: Arbitrary[C]) :Prop =
      forAll { ( c: C, a: A) => l.get (l.set (a) (c)) == a }

    def GetPut[C,A] (l: Lens[C,A]) (implicit aC: Arbitrary[C]) :Prop =
      forAll { c: C => l.set (l.get(c)) (c) == c }

    def PutPut[C,A] (l: Lens[C,A])
      (implicit aA: Arbitrary[A], aC: Arbitrary[C]) :Prop =
      forAll { (a:A, a1: A, c: C) => l.set (a1) (l.set (a) (c)) == l.set (a1) (c) }

    "nullOption put-get" in check { PutGet[String,Option[String]] (nullOption) }
    "nullOption get-put" in check { GetPut[String,Option[String]] (nullOption) }
    "nullOption put-put" in check { GetPut[String,Option[String]] (nullOption) }

  }


}



