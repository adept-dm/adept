import sbt._
import sbt.Keys._


object Dependencies {

  val ivy =         Seq(libraryDependencies +=  "org.apache.ivy"  %       "ivy"      %       "2.3.0-rc1")

  val git =         Seq(libraryDependencies +=  "org.eclipse.jgit" % "org.eclipse.jgit" % "2.3.1.201302201838-r")

  val logback =     Seq(libraryDependencies +=  "ch.qos.logback" % "logback-classic" % "1.0.9")

  val json4s =      Seq(libraryDependencies +=  "org.json4s" %% "json4s-native" % "3.2.4")

  val akka =        Seq(libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.5",
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")
    
  val spray =       Seq(libraryDependencies += "io.spray" % "spray-can" % "1.0-M7", //2.9.3 ==1.0x 2.10 == 1.1x currently
    resolvers += "spray repo" at "http://repo.spray.io")

  //TEST:
  val scalaTest =   Seq(libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b" % "test")

}
