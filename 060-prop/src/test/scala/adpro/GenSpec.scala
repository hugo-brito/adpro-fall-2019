// Wasowski, Fu, Advanced Programming, IT University of Copenhagen
// Change package to adpro.SOLUTIONS to test teacher's solutions
package adpro.testing

import org.scalatest.{FreeSpec,Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Gen => SCGen}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import fpinscala.state.RNG._
import fpinscala.state._

object Util {

  // cheating slightly as we are using another generator to seed ours
  def genRNG (implicit arbLong: Arbitrary[Long]): SCGen[RNG] =
    for { n <- arbLong.arbitrary } yield Simple (n)

  def genRNG2 (implicit arbLong: Arbitrary[Long]): SCGen[(RNG,RNG)] =
    for {
      rng1 <- genRNG
      rng2 <- genRNG suchThat { _ != rng1 }
    } yield (rng1 -> rng2)

  implicit val arbRNG: Arbitrary[RNG] = Arbitrary(genRNG)

}

class GenSpec extends FreeSpec with Matchers with PropertyChecks {

  import Util._

  "Exercise 7 (Prop, no tests so far; make sure it compiles; we compose Props extensively in the mini project next week)" - { }

  "Exercise 6 (union)" - {
    "union is idempotent (a simple scenario)" in {
      val g = Gen.unit(42)
      for { n <- (g union g).listOfN (5) } yield { n shouldBe List(42,42,42,42,42) }
    }
  }

  "Exercise 5 (listOfOn generalized size)"  - {
    "should give a list of the right size and contents" in {
      val h = Gen.unit (271)
      for { l <- Gen.unit[Double](42.311).listOfN (h) }
      yield {
        all (l) shouldBe 42.311
        l should have size 271
      }
    }
  }

  "Exercise 4 (flatMap)" - {

    "A simple fixed flatMap scenario" in {
      val g = Gen.unit (42314)
      val h = Gen.unit (2)
      for { l <- h flatMap (g listOfN _) }
      yield { l shouldBe List (42314, 42314) }
    }

  }

  "Exercise 3 (listOfN with fixed size)" - {

    "should compile" in {
      """val r3: List[List[Boolean]] =
                  (Gen.boolean.listOfN(3) toStream 42 take 5).toList
      """ should compile
    }

    "should give a list of the right size and contents" in {
      for { l <- Gen.unit[Double](42.314).listOfN (42) }
      yield {
        all (l) shouldBe 42.314
        l should have size 42
      }
    }

  }

  "Exercise 2 (unit, boolean, and double)" - {
    "simple test cases" in {
      val r1 = Gen.unit(3.14).toStream(42).take(5).toList
      all (r1) shouldBe 3.14

      // These two are not really tests for the time being but let's check
      // if they type check
      "val r2: List[Boolean] = Gen.boolean.toStream(42).take(5).toList" should compile
      "val r3: List[Double] = Gen.double.toStream(42).take(5).toList" should compile
    }
  }

  "Exercise 1 (choose)" - {

    "Generate numbers must be in range [|n|;|n|+|m|+1)" in {
      forAll (SCGen.choose (0,10000) -> "start",
              SCGen.choose (0,10000) -> "m") {
        (start: Int, m: Int) =>
          val stopExclusive = start + m + 1
          val g = Gen.choose (start, stopExclusive)
          forAll ("rng") { (rng: RNG) =>
            all (g.toStream (rng) take 100) should be >= start
            all (g.toStream (rng) take 100) should be < stopExclusive
          }
      }
    }
  }

}
