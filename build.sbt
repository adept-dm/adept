//import adept.sbt._

scalaVersion := "2.11.1"

scalacOptions += "-feature"

scalacOptions += "-deprecation"

val scalatestDep = "org.scalatest" %% "scalatest" % "2.0" % "test"

incOptions := incOptions.value.withNameHashing(true)

val adeptVersion = "0.9.2.5"
val jvmTarget = "1.6"
val jacksonDep = "com.fasterxml.jackson.core" % "jackson-core" % "2.4.0"

lazy val adeptLockfile = project.in(file("adept-lockfile")).settings(
  name := "adept-lockfile",
  version := adeptVersion,
  organization := "com.adepthub",
  scalacOptions += s"-target:jvm-$jvmTarget",
  javacOptions ++= Seq("-target", jvmTarget, "-source", jvmTarget),
  autoScalaLibrary in Test := false,
  crossPaths in Test := false, 
  libraryDependencies ++= Seq(
    jacksonDep,
    scalatestDep
   )
)//.settings(AdeptPlugin.adeptSettings: _*)

lazy val adeptCore = project.in(file("adept-core")).settings(
  name := "adept-core",
  version := adeptVersion,
  organization := "com.adepthub",
  scalacOptions += "-target:jvm-"+jvmTarget,
  javacOptions ++= Seq("-target", jvmTarget, "-source", jvmTarget),
  //jgit
  resolvers += "Jgit Repository" at "https://repo.eclipse.org/content/groups/releases/",
  //play?
  resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  resolvers += Resolver.url("Typesafe Repository (non maven compat)",  url("http://repo.typesafe.com/typesafe/releases"))(Resolver.ivyStylePatterns),
  libraryDependencies ++= Seq(
     "org.eclipse.jgit" % "org.eclipse.jgit" % 	"3.1.0.201310021548-r",
     "net.sf.ehcache" % "ehcache-core" % "2.6.6", //needed by adept.repository.RepositoryEngine
     "javax.transaction" % "jta" % "1.1", //needed by ehcache
     jacksonDep,
     scalatestDep)
).dependsOn(adeptLockfile) //.settings(AdeptPlugin.adeptSettings: _*)

