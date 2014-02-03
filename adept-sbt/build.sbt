sbtPlugin := true

name := "adept-sbt"

lazy val root = project
  .in( file(".") )
  .dependsOn(RootProject( file("..") ))

