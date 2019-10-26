// wasowski, Advanced Programming, IT University of Copenhagen
package fpinscala

import org.scalatest.{FreeSpec,Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary

class ExercisesSharedSpec (M :ExercisesInterface) extends
    FreeSpec with Matchers with PropertyChecks {

  val COMP = (a: Int, b: Int) => (a <= b)

  val genIntArray: Gen[Array[Int]] = Gen.choose(0,10000)
    .flatMap { n => Gen.listOfN[Int](n, arbitrary[Int]) }
    .map { _.toArray }

  def fpinscalaList[A] (l: scala.collection.immutable.List[A]): List[A] =
    fpinscala.List (l: _*)

  def scalaList[A] (l: fpinscala.List[A]): scala.collection.immutable.List[A] =
    l match {
      case Nil => scala.collection.immutable.Nil
      case Cons(h,t) => h :: scalaList(t)
    }

  implicit def genList[A] (
    implicit arbScalaList: Arbitrary[scala.collection.immutable.List[A]])
    : Gen[fpinscala.List[A]] =
      arbScalaList.arbitrary.map { fpinscalaList _ }

  implicit def arbitraryList[A] (
    implicit arbScalaList: Arbitrary[scala.collection.immutable.List[A]])
    : Arbitrary[fpinscala.List[A]] = Arbitrary ( genList )

  def listOfNInts (n: Int) (implicit arb: Arbitrary[Int]): Gen[List[Int]] =
    Gen.listOfN[Int] (n, arb.arbitrary)
      .map { fpinscalaList _ }

  def nonEmptyIntList (implicit arb: Arbitrary[Int]): Gen[List[Int]] =
    for {
      n <- Gen.choose (1,500)
      g = Gen.listOfN[Int] (n, arb.arbitrary)
      l <- g
    } yield fpinscalaList(l)

  "Exercise 26 (pascal)" - {

    "test cases from the exercise text" in {

      M.pascal (1) should equal (List (1))
      M.pascal (2) should equal (List (1,1))
      M.pascal (3) should equal (List (1,2,1))
      M.pascal (4) should equal (List (1,3,3,1))

    }

  }



  "Exercise 25 (hasSubsequence)" - {

    "test cases from the exercise" in {
      val l = List(1,2,3,4)

      M.hasSubsequence (l, List(1,2)) shouldBe true
      M.hasSubsequence (l, List(2,3)) shouldBe true
      M.hasSubsequence (l, List(4)) shouldBe true
    }

    "empty list have no Nil subsequence" in {
      forAll (nonEmptyIntList -> "l") { (l: List[Int]) =>
        M.hasSubsequence (Nil, l) shouldBe false
      }
    }

    "Nil is a subsequence of every list" in {
      forAll ("l") { (l: List[Int]) =>
        M.hasSubsequence (l,Nil) shouldBe true
      }
    }

    "concatenation introduces a subsequence" in {
      forAll ("l1", "l2") { (l1: List[Int], l2: List[Int]) =>
        M.hasSubsequence (M.concat (List(l1,l2)), l2) shouldBe true
        M.hasSubsequence (M.concat (List(l1,l2)), l1) shouldBe true
        M.hasSubsequence (M.concat (List(l1,l2,l1)), l2) shouldBe true
      }
    }
  }



  "Exercise 24 (zipWith)" - {
    "zipWith plus is add" in {
      forAll ("l","r") { (l: List[Int], r: List[Int]) =>
        M.zipWith[Int,Int,Int] {_+_} (l, r) should equal (M.add (l) (r))
      }
    }

    "zipWith {_-_} (l) (l) gives a list of zeros of the same length as l" in {
      forAll (nonEmptyIntList -> "l") { (l: List[Int]) =>
        val lz = M.zipWith[Int,Int,Int] { _ - _ } (l,l)
        scalaList(lz) should contain only 0
        M length lz should equal (M length l)
      }
      M.zipWith[Int,Int,Int] { _-_ } (Nil,Nil) should equal (Nil)
    }
  }



  "Exercise 23 (add)" - {
    "the test case from the exercise " in {
      M.add (List(1,2,3)) (List(4,5,6,7)) should equal (List(5,7,9))
    }
  }



  "Exercise 22 (filter1)" - {
    "we check if filter and filter1 are equivalent. You have to manually check if you are using flatMap, NO recursion, NO fold, NO patter matching, and NO filter" in {
      val p: Int => Boolean = x => x % 3 == 1
      forAll ("l") { (l: List[Int]) =>
        M.filter1 (l) (p) should equal (M.filter (l) (p))
      }
    }
  }



  "Exericse 21 (flatMap)" - {
    "the test case from the text of the exercise flatMap (List(1,2,3)) { i => List(i,i) }" in {
      M.flatMap (List(1,2,3)) {i => List(i,i)} should equal (List(1,1,2,2,3,3))
    }

    "flatMap with identity is concat" in {
      forAll ("l") { (l: List[List[Int]]) =>
        M.flatMap (l) {x => x} should equal (M concat l)
      }
    }
  }



  "Exercise 20 (filter)." - {
    "Check behavior against the standard library of Scala" in {
      val even: Int => Boolean = x => x % 2 == 0
      forAll ("l") { (l: List[Int]) =>
        scalaList(M.filter (l) (even)) should equal (scalaList(l) filter even)
      }
    }
  }



  "Exercise 19 (concat)" - {
    "Check behavior against the standard library of Scala" in {
      forAll ("l") { (l: List[List[Int]]) =>
        val l_scala = scalaList(l)  map { scalaList _ }
        scalaList(M.concat (l)) should equal (l_scala.flatten)
      }
    }
  }



  "Exercise 18 (foldLeft1). We only check if the function behavior is correct. You need to double check yourself that you are NOT using reverse, using foldRight, and NOT using recursion." - {

    withClue ("The test simply checks if foldLeft1 behaves like foldLeft") {

      "check a sequence of subtractions against foldLeft" in {
        forAll ("l") { (l:List[Int]) =>
          forAll (Gen.choose(-50000,50000) -> "z") { (z: Int) =>
            M.foldLeft1 (l,z) {_-_} should equal (scalaList(l).foldLeft (z) {_-_})
          }
        }
      }

      "check if the order of evaluations is the same" in {
        forAll ("l") { (l:List[Int]) =>
          scalaList(M.foldLeft1[Int,List[Int]] (l,Nil) { case (z,n) => Cons(n,z) }) should
          equal (scalaList(l).foldLeft[scala.collection.immutable.List[Int]] (scalaList(Nil)) { case (z,n)=>n::z } )
        }
      }
    }
  }



  "Exercise 17 (foldRight1). We only check if the function behavior is correct. You need to double check yourself that you are using reverse and foldLeft, and NOT using recursion." - {

    withClue ("The test simply checks if foldRight1 behaves like foldRight") {

      "check a sequence of subtractions against foldRight" in {
        forAll ("l") { (l:List[Int]) =>
          forAll (Gen.choose(-50000,50000) -> "z") { (z: Int) =>
            M.foldRight1 (l,z) {_-_} should equal (scalaList(l).foldRight (z) {_-_})
          }
        }
      }

      "check if the order of evaluations is the same" in {
        forAll ("l") { (l:List[Int]) =>
          scalaList(M.foldRight1[Int,List[Int]] (l,Nil) (Cons(_,_))) should equal (
              scalaList(l).foldRight[scala.collection.immutable.List[Int]] (scalaList(Nil)) {_::_})
        }
      }
    }
  }



  "Exercise 16 (reverse). We are testing only the functional behavior.  You have to check MANUALLY that you are NOT using recursion, but just call one of the fold functions with the right arguments." - {

    "reverse of a Nil is Nil" in { M.reverse (Nil) should be (Nil) }

    "reverse of a singleton is the same list" in {
      forAll ("n") { (n:Int) =>
        val l = List(n)
        M.reverse (l) should equal (l)
      }
    }

    "reversing a list twice is identity" in {
      forAll ("l") { (l:List[Int]) =>
        M.reverse (M.reverse (l)) should equal (l)
      }
    }

    "reverse (List(1,2,3,4,42)) == List (42,4,3,2,1)" in {
      M.reverse (List(1,2,3,4,42)) should be (List(42,4,3,2,1))
    }

    "reverse an asymetric list is not identity" in {
      forAll ("l") { (l:List[Int]) =>
        val l1 = Cons(43,M.reverse(Cons(42,l)))
        M.reverse (l1) should not equal (l1)
      }
    }

  }



  "Exercise 15 (product and length1). We test the functional specification here only. You have to MANUALLY check that you are using foldLeft and are NOT using recursion, and NOT using foldRight." - {

    "length of empty list is zero" in { M.length1(Nil) should be (0) }

    "product of empty list is 1" in { M.product (Nil) should be (1) }

    "product of a singleton list is equal to the value in the only cell of the list" in {
      forAll ("n") { (n:Int) =>
        M.product (List(n)) should be (n)
      }
    }

    "product behaves like the product in the standard library" in {
      forAll ("l") { (l: List[Int]) =>
        M.product (l) should be (scalaList(l).product) }
    }

    "length behaves like the length in the standard library" in {
      forAll ("l") { (l: List[Int]) =>
        M.length1 (l) should be (scalaList(l).length) }
    }
  }



  "Exercise 14 (foldLeft). " - {

    "foldLeft returns the initial value on empty list" in {
      forAll ("f") { (f:(Int,String)=>Int) =>
        M.foldLeft (Nil, 42)  (f) should be (42)
      }
    }

    "check if -10 == (((0-1)-2)-3)-4 using foldLeft" in {
      val l = List (1,2,3,4)
      M.foldLeft (l,0) (_ - _) should be (-10)
    }

    "for sanity also check if -2 == 1-(2-(3-(4-0))) using foldRight" in {
      val l = List (1,2,3,4)
      List.foldRight (l,0) (_ - _) should be (-2)
    }

    "check if foldLeft computes summation alright (to rule out anomalies in special cases)" in {
      forAll ("l") { (l: scala.collection.immutable.List[Int]) =>
        M.foldLeft (fpinscalaList(l),0) (_+_) should be (l.sum)
      }
    }

  }



  "Exercise 13 (length)" - {
    "we test length against scala's List.length. You have to check yourself if you are using foldRight, NOT using recursion, and NOT using foldLeft ..." in {
      forAll ("l") { (l: List[Int]) =>
        M.length(l) should be (scalaList(l).length) }
    }
  }



  "Exercise 12 (init)" - {

    "init on an empty list throws an exception" in {
      withClue ("We flag this bug, if you throw no exception, or if you throw NotImplementedError using '???' as this should be reserved for unimplemented code, not for runtime errors\n") {
        the [Object] thrownBy (M.init (Nil)) should not matchPattern {
          case _: NotImplementedError => }
      }
    }

    "forall l, a: init (l ++ List(a)) == l" in {
      forAll ("l","n") { (l :scala.collection.immutable.List[Int],n: Int) =>
        M.init (fpinscalaList(l ++ scala.collection.immutable.List(n))) should be (fpinscalaList(l))
      }
    }
  }



  "Exercise 11 (dropWhile)" - {

    "dropWhile with a constant predicate true should give an empty list" in {
      forAll ("l") { (l: List[Int]) =>
        M.dropWhile[Int] (l, _ => true) shouldBe Nil
      }
    }

    "dropWhile with a a constant predicate false should not change the list" in {
      forAll ("l") { (l: List[Int]) =>
        M.dropWhile[Int] (l, _ => false) should be theSameInstanceAs l
      }
    }

    "dropWhile with a random predicate should give a list that satisfies the predicate" in {
      forAll ("l") { (l: List[Int]) =>
        forAll (Gen.choose (2,15) -> "n") { (n: Int) =>
          val tl: List[Int] = M.dropWhile[Int] (l, _%n != 0)
          tl match {
            case Nil => true
            case Cons(h,t) =>  (h%n ==0) should be (true)
          }
        }
      }
    }

    "dropWhile on an empty list should always return an empty list" in {
      forAll ("p") { (p: Int => Boolean) =>
        M.dropWhile[Int] (Nil,p) should be (Nil) }
    }
  }



  "Exercise 10 (drop)" - {

    "drop(1) should behave exactly like tail" in {
      forAll (nonEmptyIntList -> "l") { (l: List[Int]) =>
          M.tail (l) should be theSameInstanceAs (M.drop(l,1))
      }
    }

    "tail(drop(l,n)) == drop(tail(l),n) == drop(l,n+1)" in {
      forAll (Gen.choose(1,100) -> "n") { (n: Int) =>
        forAll (listOfNInts (n*2) -> "l") { (l: List[Int]) =>
          M.drop (l,n+1) should {
            equal (M.tail (M.drop(l,n))) and
            equal (M.drop(M.tail (l),n))
          }
        }
      }
    }

    "drop(n) on a list with less than n elements throws an exception" in {
      withClue ("We flag this bug, if you throw no exception, or if you throw NotImplementedError using '???' as this should be reserved for unimplemented code, not for runtime errors\n") {
        forAll (Gen.choose(0,100) -> "n") { (n: Int) =>
          forAll (listOfNInts (n) -> "l") { (l: List[Int]) =>
            the [Object] thrownBy (M.drop (l,n+1)) should not matchPattern {
              case _: NotImplementedError => }
          }
        }
      }
    }
  }



  "Exercise 9 (tail)" - {

    "tail on empty set throws an exception" in {
      withClue ("We flag this bug, if you throw no exception, or if you throw NotImplementedError using '???' as this should be reserved for unimplemented code, not for runtime errors\n") {
        val e = the [Object] thrownBy (M.tail (Nil))
        e should not matchPattern { case e: NotImplementedError => }
      }

    }

    "tail is an inverse of Cons" in {
      withClue ("If I take any list, Cons an arbitrary element, and then drop it, I should get the list I started with") {
        forAll ("x","l") { (x: Int, l: List[Int]) =>
          M.tail (Cons(x,l)) shouldBe l }
      }
    }

    "tail should not copy the list" in {
      withClue ("Copying a list at the tail is very expensive and would lead to overloading the garbage collector when used in a loop or recursion. If this test fails and the previous one passes, it means that you are making a needless copy of the list.") {
        forAll ("x","l") { (x: Int, l: List[Int]) =>
          M.tail (Cons(x,l)) should be theSameInstanceAs l }
      }

    }

  }



  // Exercise 8 requires no programming



  "Exercise 7 (compose)" - {

    "should be associative: (compose(compose (f,g),h) == compose (f, compose(g,h))" in {
      withClue ("Poor diagnostic info. Unfortunately, hard to do for generated function values:\n") {
        forAll ("f", "g", "h") { (f :Int => Int, g: Int => Int, h: Int => Int) =>
          forAll ("n") { (n: Int)  =>
            M.compose (M.compose (f,g),h) (n) shouldBe M.compose(f, M.compose (g,h)) (n) }
        }
      }
    }

    "left composition with identity does not change the function" in {
      withClue ("Poor diagnostic info. Unfortunately, hard to do for generated function values:\n") {
        forAll ("f") { (f :String => String) =>
          forAll ("n") { (s: String)  =>
          M.compose (_: String=>String, f) (s) shouldBe f (s) }
        }
      }
    }

    "right composition with identity does not change the function" in {
      withClue ("Poor diagnostic info. Unfortunately, hard to do for generated function values:\n") {
        forAll ("f") { (f :Int => String) =>
          forAll ("n") { (n: Int)  =>
          M.compose (f, _: Int=>Int) (n) shouldBe f (n) }
        }
      }
    }

  }



  "Exercise 6 (uncurry)" - {

    "should not throw exception" in { M.curry (M.isSorted _) }

    "should not change value of the function for random functions" in {
      forAll ("f") { (f :Int => Int => Int) =>
        forAll ("n", "m") { (n: Int, m: Int) =>
          f(n) (m) shouldBe M.uncurry (f) (n,m)
        }
      }
    }

    "should not change the value of isSorted" in {
      forAll ((genIntArray,"as")) { (as: Array[Int]) =>
        M.isSorted[Int] (as.sorted, COMP) shouldBe
          (M.uncurry (M.curry(M.isSorted[Int] _))) (as.sorted, COMP)
      }
    }

  }



  "Exercise 5 (curry)" - {

    "should not throw exception" in { M.curry (M.isSorted _) }

    "should not change value of the function for random functions" in {
      forAll ("f") { (f :(Int,Int) => Int) =>
        forAll ("n", "m") { (n: Int, m: Int) =>
          f(n,m) shouldBe M.curry (f) (n) (m)
        }
      }
    }

    "should not change the value of isSorted" in {
      forAll ((genIntArray,"as")) { (as: Array[Int]) =>
        M.isSorted[Int] (as.sorted, COMP) shouldBe
          M.curry (M.isSorted[Int] _) (as.sorted) (COMP)
      }
    }

  }



  "Exercise 4 (isSorted)" - {


    "Array(1,2,3,4,5,6)" in {
      M.isSorted (Array(1,2,3,4,5,6), COMP) shouldBe true
    }

    "Array(6,2,3,4,5,6)" in {
      M.isSorted (Array(6,2,3,4,5,6), COMP) shouldBe false
    }

    "Array(1,2,3,4,5,1)" in {
      M.isSorted (Array(1,2,3,4,5,1), COMP) shouldBe false
    }

    "An array after standard sorting is sorted" in {
      forAll ((genIntArray,"as")) { (as: Array[Int]) =>
        M.isSorted[Int] (as.sorted, COMP) shouldBe true
      }
    }

  }



  "Exercise 3 (fib)" - {

    val genfib = Gen.choose(1, 30)

    "The first Fibonacci number fib(1) is zero" in { M.fib (1) shouldBe 0 }
    "The second Fibonacci number fib(2) is one" in { M.fib (2) shouldBe 1 }
    "The third Fibonacci number fib(3) is one"  in { M.fib (3) shouldBe 1 }
    "The fourth Fibonacci number fib(4) is two" in { M.fib (4) shouldBe 2 }

    "Each Fibonacci number is a sum of the two previous numbers" in {
      forAll ((genfib,"n")) { (n: Int) =>
        whenever (n > 2) {
          M.fib (n) shouldBe M.fib (n-1) + M.fib (n-2)
        }
      }
    }

    "Fibonacci numbers are positive (for positive arguments)" in {
      forAll ((genfib,"n"))  { (n: Int) => M.fib (n) should be >= 0 }
    }

  }


}
