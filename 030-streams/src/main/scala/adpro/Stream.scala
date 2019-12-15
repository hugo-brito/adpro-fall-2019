// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package adpro

sealed trait Stream[+A] {
	import Stream._

	def isEmpty: Boolean = this.equals(Stream.empty)

	def headOption () :Option[A] = this match {
		case Empty => None
		case Cons(h,t) => Some(h())
	}

	def tail :Stream[A] = this match {
		case Empty => Empty
		case Cons(h,t) => t()
	}

	def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
		case Empty => z
		case Cons (h,t) => f (h(), t().foldRight (z) (f))
		// Note 1. f can return without forcing the tail
		// Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
		// if f requires to go deeply into the stream. So folds sometimes may be
		// less useful than in the strict case
	}	

	// Note 1. eager; cannot be used to work with infinite streams. So foldRight
	// is more useful with streams (somewhat opposite to strict lists)
	def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
		case Empty => z
		case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
		// Note 2. even if f does not force z, foldLeft will continue to recurse
	}

	def exists (p : A => Boolean) :Boolean = this match {
		case Empty => false
		case Cons (h,t) => p(h()) || t().exists (p)
		// Note 1. lazy; tail is never forced if satisfying element found this is
		// because || is non-strict
		// Note 2. this is also tail recursive (because of the special semantics
		// of ||)
	}

	//Exercise 2
	def toList: List[A] = this match {
		case Empty => Nil
		case Cons(h,t) => h.apply() :: t().toList 
	}

	/**
	Feedback:
	Ex 2,3.
	The solutions are correct, i.e. they do the job. I would put the Cons
	first so, for example toList would look like this:

    def toList2: List[A] = this match {
		case Cons(h,t) => h()::t().toList
		case _ => List()
	}

	It depends on the nature of the Stream, but if we are expecting for it
	to have a lot of elements, it makes sense to first match if we have
	an element and then if the Stream is empty, as it is most likely we
	will hit the first case more often.
	The _ in the second is just because we do not care about it, we know
	it is Empty (as there are only two constructors), however your solution
	makes this explicit.
	*/

	//Exercise 3
	def take(n: Int): Stream[A] = {
		// take(n) for returning the first n elements of a Stream
		if (n > 0) {
			this match {
				case Empty => Empty
				case Cons(h,t) => cons(h(), t().take(n-1))
			}
		} else {
			Empty
		}
	}

	/**
	Feedback:
	Cases can also be enriched with conditions so in Ex 3 you may write

	def take2(n: Int): Stream[A] = this match {
		case Cons(h,t) if n > 0 => Cons(h, () => t().take(n-1))
		case _ => Empty (or empty)
	}

	// It will match the first case when n > 0. If n = 0 then it will fall to the
	// next case that is the same when the Stream is Empty.
	*/

	def drop(n: Int): Stream[A] = this match {
		//drop(n) for skipping the first n elements of a Stream
		case Empty => Empty
		case _ if (n <= 0) => this
		case Cons(_,t) => t().drop(n-1)
	}

	/* When trying the following:
	naturals.take(1000000000).drop(41).take(10).toList
	We get the expected result of (without it overflowing):
	List(41, 42, 43, 44, 45, 46, 47, 48, 49, 50)
	This is because our Streams are lazy evaluated, hence it will
	not compute all the values in the .take(1000000000), it will 
	only (eagerly) evaluate the values when we do the toList method.
	*/

	//Exercise 4
	def takeWhile(p: A => Boolean): Stream[A] = this match {
		// takeWhile (p) for returning all starting elements of a Stream that 
		// match the given predicate p
		case Empty => Empty
		case Cons(h,t) => {
			if ( p (h() ) ) cons(h(), t().takeWhile (p) )
			else t().takeWhile(p)
		}
	}

	/**
	Feedback
	Ex. 4
	takeWhile 'overshoots' :) The intuition behind it is to take elements from the stream
	while they fulfill a predicate and stop as soon as we see the first element that does
	not.  With this intuition, naturals.takeWhile(n => n > 0) should return Empty
	But this is not the case in your solution.
	Another example you can try is naturals.takeWhile(n => n%2 == 0) this should be Stream(0)
	*/

	// Testing our implementation on the following test case:
	// naturals.takeWhile.(_<1000000000).drop(100).take(50).toList
	// The reason is that streams are lazily evaluated, much like the above answer

	//Exercise 5
	def forAll(p: A => Boolean): Boolean = this match {
		// checks that all elements in this Stream satisfy a given
		// predicate. Terminate the traversal as soon as it encounters
		// a non-matching value.
		case Empty => false
		case Cons(h,t) => {
			if (p ( h() ) ) t().forAll(p)
			else false
		}
	}

	/**
	Feedback
	Ex. 5
	Let's assume we have val s = Stream(0,2,4,6). So s.forAll(n => n%2 == 0) should return
	true, however it returns false why?. Moreover I say that, no matter what the stream
	and predicate are, forAll will -always- return false. Why?
	*/

	// This should succeed: naturals.forAll (_ < 0)
	// because it stops right on the first iteration, as naturals will
	// always be more than 0.

	// This should crash: naturals.forAll (_ >=0) . Explain why.
	// It crashes because it's an infinite Stream and it will never reach
	// the largest number, as it will evaluate forever or in our case
	// until the stack overflows (like which most computers).

	/*
	Because of the nature of infinte streams, they are not finite, therefore,
	if we do not know the results we risk a stackoverflow and if we do, there
	isn't really a point to us using them, as we know it will return false.
	However, if our stream is finite the forAll will not risk stackoverflow and
	we can use it insdiscriminately.
	*/


	//Exercise 6
	def takeWhile2(p: A => Boolean): Stream[A] =
		this.foldRight[Stream[A]] (Empty) ((h, acc) =>  if( p (h) ) cons(h, acc) else acc)
	// The test in Exercise 4 still holds true for this function.

	/**
	Feedback
	Ex. 6
	Same problem as before, it 'overshoots' :(
	*/

	//Exercise 7
	def headOption2 () :Option[A] = 
		this.foldRight[Option[A]] (None: Option[A]) ((h , acc) => Some(h))

	/**
	Feedback:
	Ex. 7
	You can drop arguemnts to lambda functions that you are not going to use on the right hand side, so
	we can write your solution:
	this.foldRight[Option[A]] (None: Option[A]) ((h , acc) => Some(h))
	to this:
	this.foldRight[Option[A]] (None: Option[A]) ((h , _) => Some(h))
	*/		

	//Exercise 8 The types of these functions are omitted as they are a part of the exercises
	def map[B] (f:A => B): Stream[B] = 
		this.foldRight (empty[B]) ((ele, acc) => cons( f(ele), acc) )

	def filter (f: A => Boolean): Stream[A] = 
    	this.foldRight (Empty:Stream[A]) ((ele:A, acc) =>  if( f(ele) ) cons(ele, acc) else acc)
	
	def append[B >: A] (that: Stream[B]): Stream[B] =
		this.foldRight (that) ((e, acc) => cons(e, acc))
		// Should be non-strict in its argument, which it is as it does not
		// go over the that stream.

	def flatMap[B] (f: A => Stream[B]) : Stream[B] =
		this.foldRight (empty[B]) ((ele, acc) => f(ele) append acc)
	/*
	On both tests cases we experience stackOverFlow, not sure whether there
	is something wrong with our implementation of if it is just that costly.
	Test case: naturals.flatMap (to _).take (100).toList
	Test case: naturals.flatMap (x =>from (x)).take (100).toList
	*/

	// Exercise 09
	// Put your answer here:
	// It is efficient for streams due to the fact that these are acting as lazy lists, meaning 
	// that the proposed implementation does not require the entire collection to be evaluated, 
	// but rather just enough for the result to be found. Whereas such a implementation for lists 
	// would require the entire collection to be evaluated by the filter function.

	/**
	Feedback:
	Ex. 9
	Good answer, I'm not sure if streams are 'acting as lazy lists' but I do get the
	intuition :)
	I would rewrite your answer as follows:
	Original version, things between [ ] I would remove and things between ( ) I would add
	It is efficient for streams due to the fact that [these are acting as lazy lists, 
	meaning that] the proposed implementation does not require the entire collection to be
	evaluated, but rather just enough for the result to be found. Whereas [such a
	implementation for] (on) lists (it) would require the entire collection to be evaluated
	by the filter function.
	'Stripped' version
	It is efficient for streams due to the fact that 
	the proposed implementation does not require the entire collection to be evaluated, 
	but rather just enough for the result to be found. Whereas on lists it 
	would require the entire collection to be evaluated by the filter function.
	*/

	// Exercise 10
	// Put your answer here:
	// For Exercise 10, please refer to the object below

	//Exercise 11, please refer to the object below

	//Exercise 12 can also be found in object

	//Exercise 13
	def map2[B] (f:A => B): Stream[B] = { 
		unfold (this) { (state) => 
		  	state match {
				case Empty => None
				case Cons(h,t) => Some((f (h()), t()))
		  	}
		}
	}

	def take2(n: Int): Stream[A] = unfold ((this, n)) { state => 
		val (stream, i) = state
		stream match {
			case Empty => None
			case Cons(h,t) if i == 1 => Some((h(), (Empty, 0)))
			case Cons(h,t) if (i > 1) => Some((h(), (t(), i-1)))
		}
	}

	def takeWhile3 (p: A => Boolean): Stream[A] = unfold (this) { state =>
		state match {
			case Cons(h,t) if (p (h()) ) => Some((h(), t()))
			case _ => None
		}
	}

	def zipWith2[B,C] (f: (A,B) => C) (that :Stream[B]) :Stream[C] = {
		unfold ((this,that)) { state =>
			state match {
				case (Cons (h1, t1), Cons (h2, t2)) => Some((f (h1(), h2()), (t1(), t2())))
				case _ => None
			}
		}
	}
}

// What should be the result of this?
// naturals.map (_%2==0).zipWith[Boolean,Boolean] (_||_) (naturals.map (_%2==1)).take(10).toList

// list with 10 true

case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]

object Stream {

	def empty[A]: Stream[A] = Empty

	def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def even_from (n: Int): Stream[Int] = from(n).filter (_%2==0)

	def apply[A] (as: A*) :Stream[A] =
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))
	// Note 1: ":_*" tells Scala to treat a list as multiple params
	// Note 2: pattern matching with :: does not seem to work with Seq, so we
	//         use a generic function API of Seq


	// Exercise 1
	// Altered in accordance to discussion on LearnIT
	// https://learnit.itu.dk/mod/hsuforum/discuss.php?d=89
	def from(n:Int):Stream[Int]=cons(n,from(n+1))

	def to(n:Int):Stream[Int]=  {
		if (n == 0) empty
		else cons(n,to(n-1))
	}
	// Previously made to function, that returns a Stream that goes from 0 to n-1.
	// def to(n :Int) :Stream[Int] = {
	//    @annotation.tailrec
	//    def aux(i :Int, acc :Stream[Int]) :Stream[Int] = if (i >= 0) aux(i-1, cons(i, acc)) else acc
	//    aux (n-1, empty)
	// }

	val naturals: Stream[Int] = from(0)
	//Exercise 10
	def fib :Stream[Int] = {
		def fib3(n: Int) = {
			@annotation.tailrec
			def fib2 (i: Int, acc1: Int, acc2: Int): Int = {
				if(i >= n) acc1
				else fib2(i+1, acc1+acc2, acc1)
			}
			if (n <= 2) 1
			else fib2(2, 1, 1)
		}
		from(1).map(fib3)
	}

	//Exercise 11
	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
		val newState = f(z)
		newState match {
			case None => Empty
			case Some((currentState, nextState)) => cons(currentState, unfold (nextState) (f)) 
		}
	}
	
	def unfold1[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] =
		f(z) map ( (x) => cons(x._1, unfold (x._2) (f))) getOrElse empty
	
	//Exercise 12
	def fib1 = {
		def fib_unfold = unfold ((1,1)) { (state) =>
		val (current, next) = state
		Some ((current, (next, current + next)	))
		}
		fib_unfold
	}
	
	def from1(n: Int): Stream[Int] = unfold (n) { (state) => 
		val nextState = state + 1
		Some(state, nextState)
	}
	
	def naturals_unfold = unfold (0) { (state) =>
		val nextState = state + 1
		Some(state, nextState)
	}
	
}