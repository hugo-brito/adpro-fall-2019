package adpro

import org.scalatest.{FreeSpec,Matchers,Inside,OptionValues}
import org.scalatest.prop._
import org.scalacheck.Arbitrary
import org.scalatest.exceptions.TestFailedException

import adpro.data._

class Exam2019AutumnSpec extends FreeSpec
  with Inside
  with Matchers
  with PropertyChecks
  with OptionValues {

  import Exam2019Autumn._

  "Part 1" - {

    "A test that always passes" in {
      forAll { (n: Int) => n shouldBe n }
    }

  }

}
