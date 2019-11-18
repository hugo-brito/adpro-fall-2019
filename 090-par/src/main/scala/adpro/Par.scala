// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: 25
//
// AUTHOR1: André Kobæk
// TIME1: ??? hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: Hugo Brito
// TIME2: ??? <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: Jonas Hartmann Andersen
// TIME3: ??? <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).

package adpro
import java.util.concurrent._
import scala.language.implicitConversions

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

object Par {

  // an alias for this kind of function:
  type Par[A] = ExecutorService => Future[A]

  def run[A] (s: ExecutorService) (a: Par[A]) : Future[A] = a(s)


  case class UnitFuture[A] (get: A) extends Future[A] {
    def isDone = true
    def get (timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel (evenIfRunning: Boolean) : Boolean = false
  }

  // wrap a value in a Par[A]
  def unit[A] (a: A) :Par[A] = (es: ExecutorService) => UnitFuture(a)

  // map is shown in the book
  def map2[A,B,C] (a: Par[A], b: Par[B]) (f: (A,B) => C) : Par[C] =
    (es: ExecutorService) => {
      val af = a (es)
      val bf = b (es)
      UnitFuture (f(af.get, bf.get))
    }

  def fork[A] (a: => Par[A]) : Par[A] = es => es.submit(
    new Callable[A] { def call = a(es).get }
  )

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  // Exercise 1 (CB7.4)

  // Exercise 1. Use lazyUnit to write a function that converts any
  // function A =>B to one that evaluates its result asynchronously
  // (so it spawns a separate thread).
  // def asyncF[A,B] (f: A => B) : A => Par[B]

  def asyncF[A,B] (f: A => B) : A => Par[B] = a => lazyUnit(f(a))

  def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
    map2 (pa,unit (())) ((a,_) => f(a))

  // Exercise 2 (CB7.5) Write a function sequence that takes a list
  // of parallel computations (List[Par[B]]) and returns a parallel
  // computation producing a list (Par[List[B]]). No additional primitives
  // are required. Don’t call run, as we do not want this function
  // to execute anything yet.
    
  def sequence[A] (ps: List[Par[A]]): Par[List[A]] = 
  // the principle of using Sequence:
  // List[Option[A]] -> Option[List[A]]
  // List(Some(1),Some(2),Some(3),None)
  // Some(List(1,2,3))
  (es : ExecutorService) => {
    ps match {
      case Nil => UnitFuture(Nil:List[A])
      case (head :: tail) => 
        val a = ( head (es) ).get
        val b = ( (sequence (tail)) (es) ).get
      UnitFuture(a :: b)
    }
  }

  // Exercise 3 (CB7.6) Implement parFilter, which filters elements
  // of a list in parallel (so the predicate f is executed in parallel
  // for the various lists elements).

  // this is shown in the book:
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    // asyncF: takes a function and applies that function on a value
    // in here, we map async to the list ps, so that every element of such list will have
    // asyncF waiting for the value.
    sequence(fbs)
    // we have a List[Par[B]], we do sequence in order to get List[Par[B]]
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
     val para = parMap (as) (e => if (f(e)) Some(e) else None) // every filter is done in parallel
     map (para) (l => l.flatten) // getting rid of None's is not.
  }

  // Exercise 4: implement map3 using map2
  //   def map2[A,B,C] (a: Par[A], b: Par[B]) (f: (A,B) => C) : Par[C]

  def map3[A,B,C,D] (pa: Par[A], pb: Par[B], pc: Par[C]) (f: (A,B,C) => D) : Par[D] = {
    map2 (unit((pa, pb)), pc) ((a, c) => {
      val (a1: A, b1: B) = a
     f(a1, b1, c) })
  }

  // shown in the book

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  // Exercise 5 (CB7.11)

  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] = 
    es => {
      val intn = run(es) (n).get
      run(es)(choices(intn))
    }
  def choice[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] = es => {
    val n = if(run(es) (cond).get) unit(0) else unit(1)
    run(es)(choiceN (n) (List(t,f)))
  }
  // Exercise 6 (CB7.13)

  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] =
    es => {
      val a = run(es) (pa).get
      run(es)(choices(a))
    }

  def choiceNviaChooser[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] = chooser(n)(choices)
  def choiceViaChooser[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] = es => run(es) (chooser(if(run(es) (cond).get) unit(0) else unit(1))(List(t,f)))

  // Exercise 7 (CB7.14)

  def join[A] (a : Par[Par[A]]) :Par[A] = es => {
    val innerA = run(es) (a).get
    run (es) (innerA)
  }
  class ParOps[A](p: Par[A]) {

  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
}
