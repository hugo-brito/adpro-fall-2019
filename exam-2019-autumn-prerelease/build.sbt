name := "Simple Set Up for Exam Questions"

version := "0.3"

scalaVersion := "2.12.9"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" 

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" 

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.29"

val libraryVersion = "2.0.0"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-state"   % libraryVersion,
)

libraryDependencies += "com.cra.figaro" %% "figaro" % "5.0.0.0"

initialCommands in console := """
import com.cra.figaro.language.{Element, Constant, Flip, Universe, Select}
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous.{Beta, AtomicBeta}
import com.cra.figaro.library.atomic.discrete.{Binomial,Uniform}
import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.algorithm.sampling.{Importance}
import com.cra.figaro.algorithm.factored.{VariableElimination}
import scala.language.postfixOps
import adpro.Exam2019Autumn._
"""
