name := "adept-core"

version := "mark-2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

//jgit
resolvers += "Jgit Repository" at "https://repo.eclipse.org/content/groups/releases/"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
	//replace json4s with once released: "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"
  	// using: resolvers += Resolver.sonatypeRepo("snapshots") // needed by scala-pickling
  "org.eclipse.jgit" % "org.eclipse.jgit" % 	"3.1.0.201310021548-r",
  "net.sf.ehcache" % "ehcache-core" % "2.6.6", //needed by adept.repository.RepositoryEnginec
  "javax.transaction" % "jta" % "1.1", //needed by ehcache?
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "org.json4s" %% "json4s-native" % "3.2.5" % "compile", 
  "org.apache.ivy"  % "ivy" % "2.3.0" % "compile",
  "org.scalatest" %% "scalatest" % "2.0" % "test")
