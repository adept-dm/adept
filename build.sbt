name := "adept-core"

version := "mark-2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.2.4" % "compile",
  "org.scalatest" %% "scalatest" % "2.0.M5b" % "test")

