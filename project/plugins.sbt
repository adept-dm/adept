resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.3.2")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.10.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.2")

resolvers += Resolver.url("adepthub-sbt-plugin-releases",
  new URL("http://adepthub.github.io/adepthub-ext/releases"))(
    Resolver.ivyStylePatterns)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.url("Typesafe Repository (non maven compat)",  url("http://repo.typesafe.com/typesafe/releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.adepthub" % "adepthub-sbt" % "0.9.2.4")

