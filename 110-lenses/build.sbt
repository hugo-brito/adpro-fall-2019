import sbt._

name := "Lenses"

version := "0.0"


scalaVersion := "2.12.4"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

logBuffered in Test := false

val scalazVersion = "7.2.21"

libraryDependencies ++= Seq ( 
  "org.scalaz" %% "scalaz-core"               % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test" )

val monocleVersion = "1.5.0" 

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-macro"   % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-state"   % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-law"     % monocleVersion % "test"
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
