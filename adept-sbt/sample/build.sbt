import AdeptKeys._

adeptSettings

name := "adept-sbt-test-project"

scalaVersion := "2.10.1"

adeptRepositories += "central" -> "git@github.com:freekh/adept-central.git"

adeptDependencies := Seq(
  "com.typesafe.akka" % "akka-actor_2.10" % "2.1.1" % "test",  
  "com.typesafe.akka" % "akka-actor_2.10" % "2.1.0",  
  "org.javassist" % "javassist" % "3.16.1-GA" % "test"
)
