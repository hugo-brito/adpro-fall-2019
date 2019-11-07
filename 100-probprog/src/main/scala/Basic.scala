// Advanced Programming, Andrzej Wasowski
// Probabilistic Programming (AKA Probability is also a monad)

package adpro

import com.cra.figaro.language.{Element, Constant, Flip, Universe, Select}
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous.{Beta, AtomicBeta}
import com.cra.figaro.library.atomic.discrete.{Binomial,Uniform}
import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.algorithm.sampling.{Importance}
import com.cra.figaro.algorithm.factored.{VariableElimination}
import scala.collection.Map

object BasicProbability {

  val die:  Element[Int] = Uniform[Int] (1,2,3,4,5,6)

  // This is like Gen (the only thing is that the API wraps everything in lists
  // because the function is actually variadic, and each sample can contain more
  // than one value --- one for each distribution given as an argument.
  //
  // Importance.sampleJointPosterior (die) take 30 toList
  //
  // Slide 6

  // This is a monad, so we can map! Let's what is the probability that n is
  // prime
  val prime: Element[Boolean] = die map { n => n == 2 || n == 3 || n == 5 }

  // This is a monad, so we can use for notation as well
  val odd: Element[Boolean] = for { n <- die } yield n % 2 == 1

  // scala> Importance.probability (prime, true)
  // res0: Double = 0.5113000000000223
  //
  // scala> Importance.probability (odd,true)
  // res1: Double = 0.4942000000000171
  //
  // scala> VariableElimination.probability (prime,true)
  // res2: Double = 0.5
  //
  // scala> Importance.probability (^^(prime,odd), ( (t: (Boolean,Boolean)) => t._1 && t._2 ))
  // res14: Double = 0.33500000000000774



  // prime.observe (true)

  // scala> Importance.probability (odd,true)
  // res17: Double = 0.6664000000000052
  // changed!!
  // impure

  sealed trait Child
  case object Boy  extends Child
  case object Girl extends Child

  val S = for {
    f <- Uniform(Boy,Girl) // first child
    s <- Uniform(Boy,Girl) // second child
  } yield (f,s)

  // the same as:
  // val S = ^^(Uniform(Boy,Girl), Uniform(Boy,Girl))
  // Means: give me an uniformely distributed pair

  val E: Element[Boolean] = S map { case (Boy,Boy) => true; case _ => false }
  val F: Element[Boolean] = S map { case (f,s) => f == Boy || s == Boy }

  // Importance.probability (E, true)
  // Importance.probability (F, true)
  // F.observe (true) // yuck!
  // Importance.probability (E,true)

  // Example with Balls

  val boxA: Element[Boolean] = Flip (.5) // Uniform (true, false)
  // true if box A, otherwise box B

  val balls: Element[Boolean] = for { // true if gree
      box <- boxA
      ball <- if (box) Flip (2.0/9)  else Flip (4.0/7)
      // true if green
    } yield (ball)

  balls.observe (false)

  // Importance.probability (boxA,true)
  //
  // Expectation
  // res1: Double = 0.6473000000000004

  val X = Uniform (1,2,3,4,5,6)

  // scala> 
  // res1: com.cra.figaro.algorithm.factored.ProbQueryVariableElimination = com.cra.figaro.algorithm.factored.ProbQ
  // ueryVariableElimination@6000e77f
  //
  // scala> res1.start
  //
  // scala> res1.expectation (X) (identity)
  // res3: Double = 3.4999999999999996
  //
  // scala> res1.stop
  //
  // scala> res1.kill

}

