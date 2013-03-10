name := "adept"

scalaVersion := "2.10.0"

organization := "org.adept"

version := "0.1"

libraryDependencies += "com.h2database" % "h2" %  "1.3.170"

libraryDependencies += "com.typesafe.slick" %% "slick" % "1.0.0"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.6.4"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"

libraryDependencies += "org.apache.ivy" % "ivy" % "2.3.0"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies += "io.spray" % "spray-can" % "1.1-M7"

libraryDependencies += "com.github.scopt" %% "scopt" % "2.1.0"

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.1.0"

libraryDependencies += "com.ning" % "compress-lzf" % "0.9.7"