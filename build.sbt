name := "adept-core"

organization := "com.adept"

version := "0.9.0-SNAPSHOT"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

//jgit
resolvers += "Jgit Repository" at "https://repo.eclipse.org/content/groups/releases/"

scalaVersion := "2.10.3"

scalacOptions += "-feature"

scalacOptions += "-deprecation"

adept.sbt.AdeptPlugin.adeptSettings
