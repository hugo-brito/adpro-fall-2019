// wasowski, Advanced Programming, IT University of Copenhagen
package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them.

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecWasowski extends FreeSpec with Matchers with PropertyChecks {

  import Stream._

  // A simple converter of lists to streams
  def list2stream[A] (la :List[A]): Stream[A] =
    la.foldRight (Stream.empty[A]) (cons[A](_,_))

  // note that there is a name clash between Stream.empty and the testing
  // library, so we need to qualify Stream.empty

  // An example generator of random finite non-empty streams
  // (we use the built in generator of lists and convert them to streams,
  // using the above converter)
  //
  // 'suchThat' filters out the generated instances that do not satisfy the
  // predicate given in the right argument.
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for {
      la <- arbitrary[List[A]] suchThat { _.nonEmpty }
    } yield list2stream (la)

  "headOption" - {

    // a scenario test:

    "returns None on an empty Stream (01)" in {
      (Stream.empty.headOption) shouldBe (None)
    }


    // two property tests:

    "returns the head of a singleton stream packaged in Some (02)" in {
      forAll { (n :Int) => cons (n, Stream.empty).headOption should be (Some (n)) }
    }

    "returns the head of random stream packaged in Some (02)" in {
      // The implict makes the generator available in the context
      implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

      // This property uses our generator of non empty streams thanks to the
      // above implicit declaration
      forAll { (s :Stream[Int]) => s.headOption shouldNot be (None) }
    }

  }

}
