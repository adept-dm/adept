name := "adept-core"

organization := "com.adepthub"

version := "0.9.1-SNAPSHOT"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.url("Typesafe Repository (non maven compat)",  url("http://repo.typesafe.com/typesafe/releases"))(Resolver.ivyStylePatterns)

//jgit
resolvers += "Jgit Repository" at "https://repo.eclipse.org/content/groups/releases/"

scalaVersion := "2.10.3"

scalacOptions += "-feature"

scalacOptions += "-deprecation"

libraryDependencies ++= Seq(
	//replace json4s with once released: "org.scala-lang" %% "scala-pickling" % "0.8.0-SNAPSHOT"
  	// using: resolvers += Resolver.sonatypeRepo("snapshots") // needed by scala-pickling
  "org.eclipse.jgit" % "org.eclipse.jgit" % 	"3.1.0.201310021548-r",
  "net.sf.ehcache" % "ehcache-core" % "2.6.6", //needed by adept.repository.RepositoryEnginec
  "javax.transaction" % "jta" % "1.1", //needed by ehcache?
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "com.typesafe.play" %% "play-json" % "2.2.1", //REMOVE!
  "org.scala-sbt.ivy"  % "ivy" % "2.4.0-sbt-d6fca11d63402c92e4167cdf2da91a660d043392" % "compile",
  "org.scalatest" %% "scalatest" % "2.0" % "test")

incOptions := incOptions.value.withNameHashing(true)
