import org.scalatest.{FreeSpec,Matchers}
import org.scalatest.prop.PropertyChecks

class MyModuleSpec extends FreeSpec with Matchers with PropertyChecks {

  "square" - {

    // does not test for overflow, but this is not the point here
    "behaves like n*n" in {
      forAll ("n") { (n: Int) => MyModule.square (n) shouldBe n*n }
    }

  }

}
