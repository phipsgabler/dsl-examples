name := "Playground"

version := "1.0"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-deprecation")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"

mainClass in (Compile,run) := Some("Main")
    