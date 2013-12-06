name := "adept-core"

version := "mark-2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.2.5" % "compile",
  "org.scalatest" %% "scalatest" % "2.0" % "test")
