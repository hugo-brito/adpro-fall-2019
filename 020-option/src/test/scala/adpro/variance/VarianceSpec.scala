// wasowski, Advanced Programming, IT University of Copenhagen
package adpro.variance

// Use sbt command: testOnly adpro.variance.VarianceSpec to see the exception
//
// The test is written in Scala for convenience, but we are testing a problem in
// Java, and running a Java function

import org.scalatest.FreeSpec

class VarianceSpec extends FreeSpec {

  // The test is ignored to avoid confusion. If you want to run it, please
  // replace 'ignore' with 'in'
  "Runtime crash caused by covariance of arrays" ignore {

    // Intentionally produces java.lang.ArrayStoreException
    // (to demonstrate the problem with covariance of arrays in Java)
    adpro.variance.Variance.problem()

  }

}
