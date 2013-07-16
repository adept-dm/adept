import AdeptKeys._

adeptSettings

name := "adept-sbt-test-project"

scalaVersion := "2.10.1"

//adeptRepositories += "central" -> "git@github.com:freekh/adept-central.git"


libraryDependencies := Seq(
//  "com.typesafe.slick" % "slick_2.10" % "1.0.1",
// "play" % "play_2.10" % "2.1.2"
//"xml-apis"%"xml-apis"%"1.3.04"
//"jfree"%"jcommon"%"1.0.12"
//"javax.activation"%"activation"%"1.0.2"
//"org.apache.xmlgraphics"%"batik-parser"%"1.7"
//"oauth.signpost"%"signpost-commonshttp4"%"1.2.1.2"
//"org.slf4j" % "jul-to-slf4j" % "1.6.6"
  "play" % "play_2.10" % "2.1.2"
)

adeptDependencies := Seq(
//  "com.typesafe.akka" % "akka-actor_2.10" % "2.1.1" % "test",  
//  "com.typesafe.akka" % "akka-actor_2.10" % "2.1.0",  
//  "com.typesafe.akka" % "akka-actor_2.10" % "2.1.0",  
//  "org.javassist" % "javassist" % "3.16.1-GA" % "test"
  //"com.typesafe.slick" % "slick_2.10" % "1.0.1",
//"oauth.signpost"%"signpost-commonshttp4"%"1.2.1.2"
  "play" % "play_2.10" % "2.1.2"
)
