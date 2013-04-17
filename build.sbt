name := "adept"

scalaVersion := "2.9.2"

organization := "org.adept"

version := "0.1"

libraryDependencies += "org.apache.ivy" % "ivy" % "2.3.0"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "2.3.1.201302201838-r"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.9"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.4"

//akka:

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.5"

//spray:

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies += "io.spray" % "spray-can" % "1.0-M7" //2.9.3 ==1.0x 2.10 == 1.1x currentyly
