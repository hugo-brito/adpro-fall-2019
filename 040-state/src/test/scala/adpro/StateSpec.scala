// wasowski, Advanced Programming, IT University of Copenhagen
// change package adpro.SOLUTIONS to test teacher's solutions
package adpro

import org.scalatest.{FreeSpec,Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object Util {

  import RNG._

  // cheating slightly as we are using another generator to seed ours
  def genRNG (implicit arbLong: Arbitrary[Long]): Gen[RNG] =
    for { n <- arbLong.arbitrary } yield SimpleRNG(n)

  def genRNG2 (implicit arbLong: Arbitrary[Long]): Gen[(RNG,RNG)] =
    for {
      rng1 <- genRNG
      rng2 <- genRNG suchThat { _ != rng1 }
    } yield (rng1 -> rng2)

  implicit val arbRNG: Arbitrary[RNG] = Arbitrary(genRNG)
}

class StateSpec extends FreeSpec with Matchers with PropertyChecks {

  import Util._
  import State._

  "Exercise 11 (random_integers)" - {

    "Your random integers have the right type" in {
      "random_integers: Stream[Int]" should compile
    }

    "Your random integers do not throw exceptions (esp. '???)" in {
      noException should be thrownBy ((random_integers: Stream[Int]).take(42000))
    }

    "Your random integers seem random" in {
      val l1 = (random_integers: Stream[Int]).take (5)
      val l2 = (random_integers: Stream[Int]).drop (42000) take (5)

      (l1) should not equal (l2)
      (l1 ++ l2).toSet.size should be >= 6
    }

  }

  "Exercise 10 (state2stream)" - {

    "constant streams" in {

      forAll ("n", "m") { (n: Int, m: Int) =>
        val s = unit[Int,Int] (n)
        (state2stream[Int,Int] (s) (m).take (1000)) should contain only (n)
      }

    }

  }

  "Exercise 9" - {

    "unit" in {
      forAll ("n", "m") { (n: Int, m: Int) =>
        val s: State[Int,Int] = unit (n)
        (s.run (m)) should equal (n -> m)
      }
    }

    "map identity law" in {
      forAll  ("n", "m") { (n: Int, m: Int) =>
        val s = State[Int,String] (k => (s"[$k]",k+m))
        s.run (n) should equal (s map {s=>s} run (n))
      }
    }

    "associativity flatMap" in {

      val s1 =
        State[RNG,Int](RNG.nonNegativeInt)
          .flatMap { n => unit (n+1)  }
          .flatMap { m => unit (m-42) }
      val s2 =
        State[RNG,Int](RNG.nonNegativeInt)
          .flatMap { n => unit (n+1)
            .flatMap  { m => unit (m-42) } }

      forAll ("rng") { (rng: RNG) =>
        (s1.run (rng)) should equal (s2.run (rng))
      }

    }

    "flatMap identity law" in {

      forAll (Gen.choose(0,1000) -> "n") { (n: Int) =>
        forAll ("rng") { (rng: RNG) =>

          withClue ("right:") {
            val s1 =
              State[RNG,List[Int]] (RNG.ints (n % 4242))
                .flatMap (unit)
            val s2 = State[RNG,List[Int]] (RNG.ints (n % 4242))

            (s1.run (rng)) should equal (s2.run (rng))
          }

          withClue ("left:") {
            val s1 = unit[RNG,Int] (n)
              .flatMap { n =>
                State (RNG.ints (n % 4242)) }
            val s2 = State[RNG,List[Int]] (RNG.ints (n % 4242))

            (s1.run (rng)) should equal (s2.run (rng))
          }

        }
      }
    }

    "map vs flatMap+unit relation" in {
      forAll  ("n", "m") { (n: Int, m: Int) =>
        val s1 = State[Int,String] (k => (s"[$k]",k+m))
          .map { m => s"${m}_42" }
        val s2 = State[Int,String] (k => (s"[$k]",k+m))
          .flatMap { m => unit(s"${m}_42") }
        (s1.run (n)) should equal (s2.run (n))
      }
    }

    "map2 should give an equivalent intDouble to direct implementation" in {

      forAll ("rng") { (rng: RNG ) =>
        val indo = State (RNG.nonNegativeInt)
          .map2 (State(RNG.double)) {(_,_)}
        (indo.run (rng)) should equal (RNG.intDouble (rng))
      }
    }

    "A simple fixed scenario for sequence" in {
       forAll ("rng","l") { (rng: RNG, l: List[Int]) =>
         val s = State.sequence (l map { n => unit[RNG,Int] (n) })
         (s.run (rng)._1) should equal (l)
       }
    }

  }

}



class RNGSpec extends FreeSpec with Matchers with PropertyChecks {

  import Util._
  import RNG._

  // this test is mostly to rule out 'constant' generators

  def subsequentValuesAreDifferent[A] (f: RNG => (A,RNG), label: String = "") =
    s"[$label] Values generated one after another are different" in {
      // mail fail but extremely unlikely
      forAll ("rng") { (rng: RNG) =>
        val (i,rng1) = f (rng)
        val (j,_) = f (rng1)
        (i) should not equal (j)
      }
    }

  "Exercise 8 (flatMap, nonNegativeLessThan)" - {

    "flatMap associativity law" in {

      val r1 =
        flatMap (
          flatMap (nonNegativeInt) { n => unit (n+1) }) {
            m => unit (m-42)
          }
      val r2 =
        flatMap (nonNegativeInt) { n =>
          flatMap (unit (n+1)) { m => unit (m-42) }
        }

      forAll ("rng") { (rng: RNG) =>
        (r1 (rng)) should equal (r2 (rng))
      }

    }

    "flatMap identity law" in {
      forAll (Gen.choose(0,1000) -> "n") { (n: Int) =>
        forAll ("rng") { (rng: RNG) =>
          withClue ("right:") {
            val r1 = flatMap(ints(n % 4242) _) (unit)
            val r2 = ints (n % 4242) _
            (r1 (rng)) should equal (r2 (rng))
          }
          withClue ("left:") {
            val r1 = flatMap(unit(n))(n => ints (n % 4242) _)
            val r2 = ints (n % 4242) _
            (r1 (rng)) should equal (r2 (rng))
          }
        }
      }
    }

    "nonNegativeLessThan (n) yields numbers from 0 to n"  in {
      forAll (Gen.choose(1,10000) -> "n", genRNG -> "rng") {
        (n: Int, rng: RNG) =>
          val (m,_) = nonNegativeLessThan (n) (rng)
          m should be >= 0
          m should be <= n
      }
    }

  }


  "Exercise 7 (sequence)" - {

    "should not throw exceptions (including ???)" in {
       forAll ("rng", "l") { (rng: RNG, l: List[Int]) =>
         noException should be thrownBy (sequence (l map { unit _ }))
         val r = sequence (l map { unit _ })
         noException should be thrownBy (r (rng))
       }
    }


    "A simple fixed scenario" in {
       forAll ("rng","l") { (rng: RNG, l: List[Int]) =>
         val r = sequence (l map { unit _ })
         (r (rng)._1) should equal (l)
       }
    }

  }

  "Exercise 6 (map2)" - {

    "map2 should not throw exceptions (in particular '???' is removed)" in {
      forAll (genRNG -> "rng", Gen.choose(0,2000) -> "n") {
        (rng: RNG, n: Int) =>

          noException should be thrownBy (
            map2 (nonNegativeInt _,ints (n) _) {(_,_)})

          val rand = map2 (nonNegativeInt _,ints (n) _) {(_,_)}
          noException should be thrownBy (rand (rng))

      }
    }

    "map2 should give an equivalent intDouble" in {
      info ("""You may need to sync your intDouble with map2
              |    for this to pass, if you did something unusual
              |    in either.""".stripMargin)

      forAll ("rng") { (rng: RNG ) =>
        val indo = map2 (nonNegativeInt, double) {(_,_)}
        (indo (rng)) should equal (intDouble (rng))
      }
    }

  }


  "Exercise 5 (_double)" - {

    "_double should not throw exceptions (in particular '???' is removed)" in {
      forAll (genRNG -> "rng", Gen.choose(0,2000) -> "n") {
        (rng: RNG, n: Int) =>
          noException should be thrownBy (_double (rng))
      }
    }


    "_double and double should behave the same" in {
      info ("You have to check yourself that you are using map,\n    and NOT using recursion.")
      forAll ("rng") { (rng: RNG) =>
        (double(rng)) should equal (_double(rng))
      }
    }

  }


  "Exercise 4 (ints)" - {

    "ints should not throw exceptions (in particular '???' is removed)" in {
      forAll (genRNG -> "rng", Gen.choose(0,2000) -> "n") {
        (rng: RNG, n: Int) =>
          noException should be thrownBy (ints(n)(rng))
      }
    }

    "your implementation should compile (so you got the return type right)" in {
      "val rng = SimpleRNG(42); ints (42)(rng)" should compile
    }

    "returned lists should have the specified length" in {
      forAll ("rng") { (rng: RNG) =>
        forAll (Gen.choose(0,2000) -> "n") { (n: Int) =>
          val (l,_) = ints (n) (rng) : (List[Int],RNG)
          (l) should have length (n)
        }
      }
    }

    subsequentValuesAreDifferent (ints (42) _, "ints (42)")

  }


  "Exercise 3 (intDouble,DoubleInt)" - {

    "the implementations should compile (so you got the return type right)" in {

      "val rng = SimpleRNG(42); intDouble(rng): ((Int,Double),RNG)" should compile
      "val rng = SimpleRNG(42); doubleInt(rng): ((Double,Int),RNG)" should compile

    }

    "intDouble should not throw exceptions (in particular '???' is removed)" in {
      forAll ("rng") { (rng: RNG) =>
        noException should be thrownBy (intDouble(rng))
      }
    }

    "doubleInt should not throw exceptions (in particular '???' is removed)" in {
      forAll ("rng") { (rng: RNG) =>
        noException should be thrownBy (doubleInt(rng))
      }
    }

    subsequentValuesAreDifferent (intDouble, "intDouble")
    subsequentValuesAreDifferent (doubleInt, "doubleInt")

  }


  "Exercise 2 (double)" - {

    "generated doubles are in the interval [0,1)" in {
      forAll ("rng") { (rng: RNG) =>
        val (d,_) = double(rng)
        (d) shouldBe >= (0.0)
        (d) shouldBe < (1.0)
      }
    }

    subsequentValuesAreDifferent (double, "double")

  }


  "Exercise 1 (nonNegativeInt)" - {

    "generated integers are non-zero" in {

      forAll ("rng") { (rng: RNG) =>
        val (i,_) = nonNegativeInt (rng)
        i shouldBe >= (0)
      }

    }

    subsequentValuesAreDifferent (nonNegativeInt, "nonNegativeInt")
  }

}
