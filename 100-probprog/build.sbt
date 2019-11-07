name := "probprog"

version := "0.00"

scalaVersion := "2.12.9"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" 
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" 
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "com.cra.figaro" %% "figaro" % "5.0.0.0"

initialCommands in console := """
//import adpro.BasicProbability._
import com.cra.figaro.language.{Element, Constant, Flip, Universe, Select}
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous.{Beta, AtomicBeta}
import com.cra.figaro.library.atomic.discrete.{Binomial,Uniform}
import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.algorithm.sampling.{Importance}
import com.cra.figaro.algorithm.factored.{VariableElimination}
import scala.language.postfixOps
import adpro.BasicProbability._

"""
