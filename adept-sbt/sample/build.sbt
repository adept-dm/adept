import AdeptKeys._

adeptSettings

name := "adept-sbt-test-project"

scalaVersion := "2.10.1"

//adeptRepositories += "central" -> "git@github.com:freekh/adept-central.git"


libraryDependencies := Seq(
//  "com.typesafe.akka" % "akka-actor_2.10" % "2.1.1"
//  "play" % "play_2.10" % "2.1.0"
)

adeptDependencies := Seq(
//  "com.typesafe.akka" % "akka-actor_2.10" % "2.1.1" % "test",  
//  "com.typesafe.akka" % "akka-actor_2.10" % "2.1.0",  
//  "com.typesafe.akka" % "akka-actor_2.10" % "2.1.0",  
//  "org.javassist" % "javassist" % "3.16.1-GA" % "test"
  "com.typesafe.slick" % "slick_2.10" % "1.0.1",
  "play" % "play_2.10" % "2.1.0"
)
