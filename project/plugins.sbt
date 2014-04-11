resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.3.2")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.10.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.2")

//resolvers += Resolver.url("adepthub-sbt-plugin-releases",
//  new URL("http://adepthub.github.io/ah-sbt-plugin/releases"))(
//    Resolver.ivyStylePatterns)

//addSbtPlugin("com.adepthub" % "adepthub-sbt-plugin" % "0.9.0-SNAPSHOT")
