import adept.sbt.AdeptKeys._
import adept.sbt._

AdeptPlugin.adeptSettings

name := "adept-sbt-sample-1"

dependencyClasspath in Compile <<= AdeptKeys.adeptClasspath 

adeptMetadataLocations := Set("https://github.com/adept-test-repo1/")
