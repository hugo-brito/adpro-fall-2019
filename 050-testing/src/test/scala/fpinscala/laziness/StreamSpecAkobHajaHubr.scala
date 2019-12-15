// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: 25
//
// AUTHOR1: André Kobæk
// TIME1: 4.5 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: Hugo Brito
// TIME2: 5.5 <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: Jonas Hartmann Andersen
// TIME3: 6 <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).

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

class StreamSpecAkobHajaHubr extends FreeSpec with Matchers with PropertyChecks {

	import Stream._

	final case class FailException(private val message: String = "fail", 
		private val cause: Throwable = None.orNull) extends Exception(message, cause)


	// A simple converter of lists to streams
	def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (Stream.empty[A]) (cons[A](_,_))
	// take(n) for returning the first n elements of a Stream


	def takeNaive[A](n: Int)(a: Stream[A]): Stream[A] = {
		if (n > 0) {
			a match {
			case Empty => Empty
			case Cons(h,t) => Cons(h, () => takeNaive(n-1)(t()))
			}
		} else {
			Empty
		}
	}
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

	implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])

	def fail() = throw FailException("fail")
	def failStream[A](n: Int):Stream[A] = {
		if (n>=0) cons(fail(),failStream(n-1))
		else Empty
	}

	"headOption" - {
		// a scenario test:
		"returns None on an empty Stream (01)" in {
			(Stream.empty.headOption) shouldBe (None)
		}
		// two property tests:

		"returns the head of a singleton stream packaged in Some (02)" in {
			forAll { (n :Int) => cons (n, Stream.empty).headOption shouldBe Some (n) }
		}

		"returns the head of random stream packaged in Some (02)" in {
			// This property uses our generator of non empty streams thanks to the
			// above implicit declaration
			forAll { (s :Stream[Int]) => s.headOption != None }
		}

		// OWN TESTS AFTER THIS:
		// Property test of the head of Singleton stream, but scenario test of lazyness 
		"Lazyness of the tail" in {
			forAll { (n: Int) => 
				cons (n, throw FailException("this")).headOption shouldBe Some(n) }
		}
		// "shouldnt work for a list" in { an [FailException] should be thrownBy {
		//   cons (1, throw FailException("fail")).toList }
		// }
	}

	"take" - {

		// Scenario tests of streams containing fail 
		"lazily evaluates contents" in {
			// "Throwing an exception" is irrelevant, if the take() evaluates
			// either the head or tail, the whole thing will fail.
			val failStream = cons(fail(),cons(fail(),cons(fail(),fail())))
			failStream.take(2) should not be "Throwing an exception"
		}

		"never forces the (n+1)st heads" in {
			forAll { (n: Int) => failStream(n+1).take(n) should not be "Throwing an exception"
			}
		}

		// .toList is probably not the right way to go, but since its
		// two unique objects comparing there object references will
		// not yield the required result.
		"will return itself when doubletaking" in {
			forAll { (n: Int, s: Stream[Int]) => ( s.take(n).take(n).startsWith(s.take(n)) && s.take(n).startsWith(s.take(n).take(n))) shouldBe true}
		}
	}

	"drop" - {
		val nonNegativeInts: Gen[Int] = for (n <- Gen.choose(0, 100000000)) yield n
		"additivity holds true for positive values of n and m" in { 
			forAll(nonNegativeInts, nonNegativeInts, genNonEmptyStream[Int]) { (n: Int, m: Int, s: Stream[Int]) => 
			{ ( s.drop(n).drop(m).startsWith(s.drop(n+m)) && s.drop(n+m).startsWith(s.drop(n).drop(m))) shouldBe true }
			}
		}

		"lazily evaluates dropped elements" in {
			val failStream = cons(fail(),cons(fail(),cons(fail(),fail())))
			failStream.drop(2) should not be "Throwing an exception"
		}

		"lazily evaluates dropped elements, even when forcing stream" in {
			forAll { (s: Stream[Int]) => cons(fail(),s).drop(1).toList shouldBe s.toList
			}
		}
	}
	
	"map" - {
		"identity is a unit with map" in {
			forAll {
			(s: Stream[Int]) => s.map(i => i).startsWith(s) shouldBe true
			}
		}

		"terminates on infinite map" in {
			val stream1 = from(0).map(_*2)
			stream1 shouldBe stream1
		}
	}

	"append" - {
		"an empty stream with a empty stream is empty" in {
			Stream.empty.append(Stream.empty) shouldBe Stream.empty
		}

		"an empty stream with non-empty gives non-empty" in {
			forAll {
			(s: Stream[Int]) => (Stream.empty.append(s).startsWith(s) && s.append(Stream.empty).startsWith(s)) shouldBe true
			}
		}

		"two non empty streams should be non-empty" in {
			forAll {
			(s: Stream[Int], t: Stream[Int]) => t.append(s) should not be Stream.empty
			}
		}

		"terminates for infinite streams" in {
			val s = from(0)
			val t = s.append(s)
			t shouldBe t
		}

		"does not eagerly evaluate the tail" in {
			forAll {
			(n: Int, s: Stream[Int]) => s.append(failStream(n)) should not be "Throwing an exception"
			}
		}
		
		"evaluates the head" in {
			intercept[FailException] { 
			failStream(1).append(from(0))
			}
		}
	}
}
