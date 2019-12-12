// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: _____
//
// AUTHOR1: __________
// TIME1: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: __________
// TIME2: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// This file is compiled with 'sbt compile' and tested with 'sbt test'.
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.    The tests
// will fail for unfnished parts.  Comment such out.

// Feedback:
// - Just an FYI: since you were using the mixin-trait, you can access the fields
//   of the object being mixed in, and thus could have accessed x and y, like
//   `this.x`.
// - I prefer `filter2` to `filter`
// - Your for-comprehensions are good!
// - `sequence` is beautifully and simply implemented
// - `traverse` does not meet the "pass the list once" condition. An implementation
//   _very_ similar to that of `sequence` is possible (running through the list
//   only once).
package adpro

// Exercise  1

/* We create OrderedPoint as a trait instead of a class, so we can mix it into
 * Points (this allows to use java.awt.Point constructors without
 * reimplementing them). As constructors are not inherited, We would have to
 * reimplement them in the subclass, if classes not traits are used.  This is
 * not a problem if I mix in a trait construction time. */

trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {

	this: java.awt.Point =>

	override def compare (that: java.awt.Point): Int = {
		// https://docs.oracle.com/javase/8/docs/api/java/awt/Point.html
		if ((this.getX() < that.getX()) || ((this.getX() == that.getX()) && (this.getY() < that.getY()))) -1
		// this is smaller
		else if ((this.getX() == that.getX()) && (this.getY() == that.getY())) 0 // the same
		else 1 // that is larger that this
	}

}

// Try the following (and similar) tests in the repl (sbt console):
// val p = new java.awt.Point(0,1) with OrderedPoint
// val q = new java.awt.Point(0,2) with OrderedPoint
// assert(p < q)

// Chapter 3


sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

	def size[A] (t :Tree[A]): Int = t match {
		case Leaf (_) => 1
		case Branch (l, r) => size (l) + size (r) + 1 // plus 1 because of the branch
	}

  // Exercise 3 (3.26)

	def maximum (t: Tree[Int]): Int = t match {
		case Leaf (v) => v
		case Branch (l, r) => maximum(l).max(maximum(r))
	}

  // Exercise 4 (3.28)

	def map[A,B] (t: Tree[A]) (f: A => B): Tree[B] = t match {
		case Leaf (v) => Leaf(f(v))
		case Branch (l, r) => Branch (map (l) (f), map (r) (f))
	}

  // Exercise 5 (3.29)

	def fold[A,B] (t: Tree[A]) (f: (B,B) => B) (g: A => B): B = t match {
		case Leaf (v) => g (v)
		case Branch (l, r) => f (fold (l) (f) (g), fold (r) (f) (g))
	}

	def size1[A] (t: Tree[A]): Int = fold[A, Int] (t) ((a,b) => 1 + a + b) (_ => 1)
	// the magic here is on the g. Once one gets to the value held by the leaf, one purely returns one, because
	// we're counting number of nodes.

	def maximum1[A] (t: Tree[Int]): Int = fold[Int,Int] (t) ((a,b) => a.max(b)) (i => i)
	// to learn the max between 2 integers one just uses the embedded function from the integer library .max
	// i => i is basically the announymous id function. And this is what we need once we get to a branch.
	// take the value and return it so that f can calculate the maximum

	def map1[A,B] (t: Tree[A]) (f: A=>B): Tree[B] = fold[A,Tree[B]] (t) ((l,r) => Branch(l,r)) (i => Leaf( f (i)))
	// normally one uses fold to collect everything in a final value. In this case, we want to preserve the
	// structure of the tree, and this is why f has to basically get the trees (left and right) and put them
	// back in a branch, and our g (which basically is when we get to the end of the tree) calculates f of the
	// value of the node and then puts it back in a leaf. This means we will be able to return a tree with f
	// applied to the values of the nodes.

}

sealed trait Option[+A] {

  // Exercise 6 (4.1)

	def map[B] (f: A=>B): Option[B] = this match {
		case Some(b) => Some (f (b))
		case None => None // none doesn't take any argument nor parenthesis
	}

  // You may Ignore the arrow in default's type below for the time being.
  // (it should work (almost) as if it was not there)
  // It prevents the argument "default" from being evaluated until it is needed.
  // So it is not evaluated in case of Some (the term is 'call-by-name' and we
  // should talk about this soon).

	def getOrElse[B >: A] (default: => B): B = this match {
	// [B >: A] means A is a subtype of B, e.g., B is a Collection and A is List

	// The “getOrElse” function in Scala is a feature designed to help write code
	// that avoids NullPointerExceptions.
		case Some(b) => b // the get part
		case None => (default) // the else part
	}

	def flatMap[B] (f: A=>Option[B]): Option[B] = this match {
	// runs func on every element and concatenates the results
	// as = [1,2]
	// as.flatMap (val -> [val;val]) = [1,1,2,2]
	// https://samgrayson.me/2019-08-06-monads-as-a-programming-pattern/
		case Some(b) => f (b) // example of an f function: a -> Some(a)
		case None => None
	}

	def filter (p: A => Boolean): Option[A] = this match {
	case Some(a) => if (p (a)) Some(a)
					else None
	case None => None
	}

	def filter2 (p: A => Boolean): Option[A] = this match {
		case Some(a) if (p (a)) => Some(a)
		case _ => None // anything that doesn't fall in the previous category
	}

}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // Remember that mean is implemented in Chapter 4 of the text book

	def mean(xs: Seq[Double]): Option[Double] =
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)

  // Exercise 7 (4.2)

  def variance (xs: Seq[Double]): Option[Double] = 
	for {
		m <- mean(xs)
		seqofmean = xs.map(x => (math.pow(x - m, 2.0)))
		res <- mean(seqofmean)
	} yield (res)

  // Exercise 8 (4.3)

	def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] =
	for {
		a <- ao
		b <- bo
		c = f(a, b)
	} yield (c)

  // Exercise 9 (4.4)

	def sequence[A] (aos: List[Option[A]]): Option[List[A]] = 
		aos.foldRight[Option[List[A]]] (Some(Nil)) ((a, b) => map2 (a, b) (_ :: _ ))

  // Exercise 10 (4.5)

	def traverse[A,B] (as: List[A]) (f :A => Option[B]): Option[List[B]] =
		sequence(as map (i => f(i)))

}
