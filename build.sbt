name := "crontab-parser-app"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

assemblyJarName in assembly := "cronparse.jar"
