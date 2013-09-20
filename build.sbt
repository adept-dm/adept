name := "adept-core"

version := "mark-2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.2.4" % "compile",
  "com.typesafe.akka" % "akka-actor" % "2.0.5" % "compile",
  "org.scalatest" %% "scalatest" % "2.0.M5b" % "test")

