import adept.sbt._

AdeptPlugin.adeptSettings

name := "adept-sbt-sample-1"

dependencyClasspath in Compile <<= AdeptKeys.adeptClasspath 