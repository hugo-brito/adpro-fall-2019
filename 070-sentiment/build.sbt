name := "sentiment"

version := "1.2"

scalaVersion := "2.11.12" 

scalacOptions ++= Seq("-unchecked", "-deprecation")

initialCommands in console := 
  """
  import Main._
  """

libraryDependencies += "org.apache.spark" %% "spark-sql" % "2.4.4" 

libraryDependencies += "org.apache.spark" %% "spark-mllib" % "2.4.4" 

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

// needed as per: https://spark.apache.org/docs/latest/ml-guide.html#dependencies
libraryDependencies += "com.github.fommil.netlib" % "all" % "1.1.2"
