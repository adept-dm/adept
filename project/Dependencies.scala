import sbt._
import sbt.Keys._


object Dependencies {

  val ivy =         Seq(libraryDependencies +=  "org.apache.ivy"  %       "ivy"      %       "2.3.0-rc1" % "compile")

  val git =         Seq(libraryDependencies +=  "org.eclipse.jgit" % "org.eclipse.jgit" % "2.3.1.201302201838-r"  % "compile")

  val logback =     Seq(libraryDependencies +=  "ch.qos.logback" % "logback-classic" % "1.0.9" % "compile")

  val json4s =      Seq(libraryDependencies +=  "org.json4s" %% "json4s-native" % "3.2.4" % "compile")

  val akka =        Seq(libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.5" % "compile",
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")
    
  val spray =       Seq(libraryDependencies += "io.spray" % "spray-can" % "1.0-M8.1" % "compile", //2.9.3 ==1.0x 2.10 == 1.1x currently
    resolvers += "spray repo" at "http://repo.spray.io")

  val ehcache =     Seq(libraryDependencies += "net.sf.ehcache" % "ehcache-core" % "2.6.6" % "compile")

  //TEST:
  val scalaTest =   Seq(libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b" % "test->runtime")

}
