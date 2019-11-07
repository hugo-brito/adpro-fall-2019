// wasowski, Advanced Programming, IT University of Copenhagen
// change package to adpro.SOLUTIONS to test teacher's solutions
package adpro

import org.scalatest.{FreeSpec,Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary


import com.cra.figaro.algorithm.factored.{VariableElimination}
import com.cra.figaro.algorithm.sampling.{Importance}
import org.scalatest.prop.TableDrivenPropertyChecks._

class ExercisesSpec extends FreeSpec with Matchers with PropertyChecks {

  val Err: Double = 0.03


  // n is the number of black balls (not the total number of balls)
  def analyticalP1 (n: Int): Double =
    if (n % 2 == 0)
      (1 + n/2) / (n + 1.0) // see notes below
    else 0.5

  // n = 3 (#black)
  // 1/4 + 1/2 * (3/4*2/3) = 1/4 + 1/4 = 1/2
  // 1/3 * 3/4 + (3/4*2/3*1/2) = 1/4 + 1/4 = 1/2

  // n = 4 (#black)
  // 1/5 + (4/5 * 3/4) * 1/3 + (4/5 * 3/4 * 1/3 * 1/2) = 3/5
  // 1/4 * 4/5 + (4/5 * 3/4 * 2/3) * 1/2 = 1/5 + 1/5 = 2/5
  //
  //  (1 + n div 2) / (n+1) for the first player
  //  (n div 2) / (n+1)     for the second player

  "Exercise 5" - {

    s"the posterior chance of an odd urn after Player1 wins should be greater than 1/2" in {

      Exercises.posteriorOdd should be > 0.5
      info ("Prob. that we had an odd-sized urn: ${Exercises.posteriorOdd}" )

    }

    s"blackBallsNo is uniform in range [0,${Exercises.UpperBound})" in {

      val T = Table (("#balls"), 0 to (Exercises.UpperBound-1): _*)
      val imp = Importance (5000, Exercises.blackBallsNo)

      // We are using the imperative API of Figaro to speedup tests for students
      // (The functional API is slower, not because it is functional, but
      // because how it has been designed, to re-run the sampling at every
      // query).

      imp.start
      Thread.sleep(1000)
      imp.stop

      forAll (T) { (i: Int) =>
        forAll (T) { (j: Int) =>
          val pi = imp.probability (Exercises.blackBallsNo, i)
          val pj = imp.probability (Exercises.blackBallsNo, j)
          pi shouldBe pj +- (Err)
        }
      }
    }


    s"Prob. that Paula has started given that she won, ${Exercises.BallsNo} balls in the urn" in {
      if (Exercises.BallsNo % 2 == 0)
        Exercises.probPaulaStarted shouldBe 0.5 +- Err
      else
        Exercises.probPaulaStarted should be > 0.5
    }

    "gameWonByPaula after conditioning on Paula wining should be 1.0" in {
      Importance.probability (Exercises.gameWonByPaula, Exercises.Paula) shouldBe 1.0
    }


    s"gameResult with ${Exercises.BallsNo} balls in the urn, with uniform prior should be 1/2" in {
      val p = Importance.probability (Exercises.gameResult, Exercises.Paula)
      p shouldBe 0.5 +- Err
    }



  }




  "Exercise 3" - {

    s"probPaula with ${Exercises.BallsNo} balls in the urn" in {
      val expected = analyticalP1 (Exercises.BallsNo-1)
      Exercises.probPaula shouldBe expected +- Err
    }

    s"probPeter with ${Exercises.BallsNo} balls in the urn" in {
      val expected = 1.0 - analyticalP1 (Exercises.BallsNo-1)
      Exercises.probPeter shouldBe expected +- Err
    }

  }


  "Exercise 2" - {

    "move (urn with even size)" in {

      val blackBalls =
        Table( ("number of black balls"),
               (0), (1), (2), (3), (4), (5), (6), (7) )

      forAll (blackBalls) { (n: Int) =>
        val p1 = Importance.probability (Exercises.move (Exercises.Peter, n), Exercises.Paula)
        val p2 = Importance.probability (Exercises.move (Exercises.Paula, n), Exercises.Paula)
        p1 shouldBe (1.0 - analyticalP1 (n)) +- Err
        p2 shouldBe (analyticalP1 (n)) +- Err
      }
    }
  }



  "Exercise 1" - {

    "pick" in {
      forAll (Gen.choose(0,10) -> "#black balls") { (n: Int) =>
        val expected = 1.0/(n+1.0)
        Importance.probability (Exercises.pick (n), true) shouldBe expected +- Err
      }
    }

  }


}
