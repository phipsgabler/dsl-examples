name := "dsl-examples"

version := "1.0"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-feature",
  "-language:implicitConversions",
  "-deprecation")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"

initialCommands in console := """import dsl_examples._"""
    