name := "adept"

scalaVersion := "2.10.0"

organization := "com.typesafe.adept"

version := "0.1"

libraryDependencies += "com.h2database" % "h2" %  "1.3.170"

libraryDependencies += "com.typesafe.slick" %% "slick" % "1.0.0"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.6.4"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"