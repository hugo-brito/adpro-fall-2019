// wasowski, Advanced Programming, IT University of Copenhagen
// change package to adpro.SOLUTIONS to test teacher's solutions
package adpro

import org.scalatest.{FreeSpec,Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class ExercisesSpec extends FreeSpec with Matchers with PropertyChecks {


  def genTree[A] (implicit arbA: Arbitrary[A]): Gen[Tree[A]] =
    Gen.frequency (7 -> true, 1 -> false)
    .flatMap { stop =>
      if (stop) arbA.arbitrary map { Leaf(_) }
      else for {
        l <- genTree[A]
        r <- genTree[A]
      } yield (Branch (l,r))
    }

  implicit def arbitraryTree[A] (implicit arbA: Arbitrary[A])
    : Arbitrary[Tree[A]] = Arbitrary[Tree[A]] (genTree (arbA))

  implicit def arbitraryOption[A] (implicit arbA: Arbitrary[A])
    : Arbitrary[Option[A]] = Arbitrary (Gen.oneOf (Gen.const(None), arbA.arbitrary map {Some(_)}))

  def nonEmptyIntList (implicit arb: Arbitrary[Int]): Gen[List[Int]] =
    for {
      n <- Gen.choose (1,500)
      l <- Gen.listOfN[Int] (n, arb.arbitrary)
    } yield (l)

  "Exercise 1" - {

    "the test from the exercise text" in {

      val p = new java.awt.Point(0,1) with OrderedPoint
      val q = new java.awt.Point(0,2) with OrderedPoint
      (p < q) shouldBe true
    }

    "non-negative shift preserves order" in {

      val coord = Gen.choose(-10000,+10000)
      val delta = Gen.choose(0,10000)

      forAll (coord -> "x", coord -> "y", delta -> "a", delta -> "b") {
        (x,y,a,b) =>
          val p = new java.awt.Point(x,y) with OrderedPoint
          val q = new java.awt.Point(x+a,y+b) with OrderedPoint
          (p <= q) shouldBe true
          (p > q) shouldBe false
      }

    }

  }

  "Exercise 2 (Tree size)" - {

    "a simple scenario test" in {
        Tree.size (Branch(Leaf(1), Leaf(2))) should be (3)
    }

    "a leaf should be size 1" in {
      forAll ("n") { n:Int => Tree.size (Leaf(n)) should be (1) }
    }

  }

  "Exercise 3 (Tree maximum)" - {

    "a simple scenario test" in {
      Tree.maximum (Branch(Leaf(1), Leaf(2))) should be (2)
    }

    "a singleton tree test" in {
      forAll ("n") { n:Int => Tree.maximum (Leaf(n)) should be (n) }
    }

    "a bi-node tree test" in {
      forAll ("n", "m") { (n:Int, m:Int) =>
        Tree.maximum (Branch(Leaf(m),Leaf(n))) should be (n max m) }
    }

    "a tri-node tree test" in {
      forAll ("n", "m", "o") { (n:Int, m:Int, o:Int) =>
        val M = n max m max o
        Tree.maximum (Branch(Leaf(m),Branch(Leaf(n), Leaf(o)))) should be (M)
        Tree.maximum (Branch(Leaf(m),Branch(Leaf(n), Leaf(o)))) should be (M)
      }
    }
  }

  "Exercise 4 (Tree map)" - {

    "a simple scenario test" in {

        val t4 = Branch(Leaf(1), Branch(Branch(Leaf(2),Leaf(3)),Leaf(4)))
        val t5 = Branch(Leaf("1"), Branch(Branch(Leaf("2"),Leaf("3")),Leaf("4")))
        Tree.map (t4) (_.toString) should be (t5)
        Tree.map (t4) (x => x) should be (t4)
    }

    "identity is a unit with map" in {
      forAll ("t") { (t: Tree[Int]) => Tree.map (t) (x => x) should be (t) }
    }

    "map does not change size" in {
      forAll ("t","f") { (t: Tree[Int], f: Int => Int) =>
        val t1 = Tree.map (t) (f)
        Tree.size (t1) should equal (Tree.size(t))
      }
    }

    "map is 'associative'" in {
      forAll ("t","f","g")  { (t: Tree[Int], f: Int=>Int, g: Int=>Int) =>
        val t1 = Tree.map (Tree.map (t) (f)) (g)
        val t2 = Tree.map (t) (g compose f)
        (t1) should equal (t2)
      }

    }
  }

  "Exercise 5 (Tree fold)" - {

    // right now there is no direct test for fold. We test it via size1, map1,
    // and maximum1

    withClue ("The test simply checks if size1, map1, maximum1 behave like o size, map, and maximumum\n We don't check if you are using fold. You should ensure this yourself.") {

      "size1 behaves like size (check that you use fold!)" in {
        forAll ("t") { (t: Tree[Int]) =>
          (Tree.size (t)) should equal (Tree.size1 (t))
        }
      }

      "maximum1 behaves like size (check that you used fold!)" in {
        forAll ("t") { (t: Tree[Int]) =>
          (Tree.maximum (t)) should equal (Tree.maximum1 (t))
        }
      }

      "map1 behaves like map (check that you used fold!)" in {
        forAll ("t","f") { (t: Tree[Int], f: Int => Int) =>
          (Tree.map (t) (f)) should equal (Tree.map1 (t) (f))
        }
      }
    }
  }

  "Exercise 6 (Option basics)" - {

    "Option map does nothing on None" in {
      forAll ("f") { (f:Int=>Int) => (None map f) should equal (None) }
    }

    "Option map on Some just maps the element" in {
      forAll ("n","f") { (n: Int, f:Int=>Int) =>
        (Some(n) map f) should equal (Some(f(n)))
      }
    }

    "Option getOrElse on None always does else" in {
      forAll ("m") { (m: Int) =>
        (None getOrElse m) should equal (m)
      }
    }

    "Option getOrElse on Some(n) always gives n" in {
      forAll ("n","m") { (n: Int, m: Int) =>
        (Some(n) getOrElse m) should equal (n)
      }
    }

    "Option flatMap on None does None" in {
      forAll ("f") { (f:Int=>Option[Int]) =>
        (None flatMap f) should equal (None)
      }
    }

    "Option flatMap to None gives None" in {
      forAll ("o") { (o:Option[Int]) =>
        (o flatMap { _ => None }) should equal (None)
      }
    }

    "Option flatMap on Some applies the function or fails" in {
      forAll ("n","f") { (n: Int, f:Int=>Option[Int]) =>
        (Some(n) flatMap f) should equal (f(n))
      }
    }

    "Option filter on None gives None" in {
      forAll ("p") { (p:Int=>Boolean) =>
        (None filter p) should equal (None)
      }
    }

    "Option filter with constant false predicate gives None" in {
      forAll ("o") { (o: Option[Int]) =>
        (o filter { _ => false }) should equal (None)
      }
    }

    "Option filter picks the value iff it satisfies the predicate" in {
      forAll ("n","p") { (n: Int, p: Int=>Boolean) =>
        (Some(n) filter p) should equal (if (p(n)) Some(n) else None)
      }
    }

  }

  "Exercise 7 (variance)" - {
    "simple fixed scenarios" in {
      ExercisesOption.variance (List(42,42,42)) should be (Some(0.0))
      ExercisesOption.variance (Nil) should be (None)
    }

    // TODO: add more test (variance of an arbitrary constant list, variance of
    // some controlled symmetric set)
  }

  "Exercise 8 (map2)" - {

    "any None gives None" in {
      forAll ("o","f") { (o: Option[Int], f:(Int,Int)=>Int) =>
        ExercisesOption.map2 (o, None) (f) should equal (None)
        ExercisesOption.map2 (None, o) (f) should equal (None)

      }
    }

    "two Some elements are nicely merged by f" in {
      forAll ("n","m") { (n: Int, m: Int) =>
        ExercisesOption.map2 (Some(n), Some(m)) (_-_) should equal (Some(n-m))
      }
    }

  }

  "Exercise 9 (sequence)" - {

    "some simple scenarios" in {

      ExercisesOption.sequence (Nil) should equal (Some(Nil))
      ExercisesOption.sequence (List(None)) should equal (None)
      ExercisesOption.sequence (List(Some(42))) should equal (Some(List(42)))
      ExercisesOption.sequence (List(Some(1), Some(2), Some(42))) should equal (Some(List(1,2,42)))
      ExercisesOption.sequence (List(None,    Some(2), Some(42))) should equal (None)
      ExercisesOption.sequence (List(Some(1), None,    Some(42))) should equal (None)
      ExercisesOption.sequence (List(Some(1), Some(2), None    )) should equal (None)

    }

    "A list without Nones sequences as if this was a list without options" in  {
      forAll ("l") { (l: List[Int]) =>
        val ol = l map { Some (_) }
        ExercisesOption.sequence (ol) should equal (Some(l))
      }
    }

    "A list with None sequences to None" in {
      forAll ("l") { (l: List[Option[Int]]) =>
        val ol = l ++ (None ::l)
        ExercisesOption.sequence (ol) should equal (None)
      }
    }
  }

  "Exercise 10 (traverse)" - {

    "some simple scenarios" in {
      def f (n: Int) :Option[Int] = if (n%2 == 0) Some(n) else None
      ExercisesOption.traverse (List(1,2,42)) (Some(_)) should equal (Some(List(1,2,42)))
      ExercisesOption.traverse (List(1,2,42)) (f) should equal (None)
    }

    "traversal of bottom gives bottom" in {
      forAll (nonEmptyIntList -> "l") { (l: List[Int]) =>
        ExercisesOption.traverse (l) { _ => None } should be (None)
      }
    }

    "empty traversal cannot fail" in {
      forAll ("f") { (f: Int=>Option[Int]) =>
        ExercisesOption.traverse (Nil) (f) should be (Some(Nil))
      }
    }

  }

}
