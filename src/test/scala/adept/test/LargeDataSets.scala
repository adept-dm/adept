package adept.test

import adept.test.TestDSL._

object LargeDataSets {
  def basic = Seq(
    V("com.typesafe.play/play")("version" -> Set("2.1.0"),
      "organization" -> Set("com.typesafe.play"),
      "name" -> Set("play"),
      "binary-version" -> Set("2.1"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(
        X("com.typesafe.akka/akka-actors")("binary-version" -> Set("2.1")),
        X("org.scala-lang/scala-library")("binary-version" -> Set("2.10"))),
    V("com.typesafe.play/play")("version" -> Set("2.2.0"),
      "organization" -> Set("com.typesafe.play"),
      "name" -> Set("play"),
      "binary-version" -> Set("2.2"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(
        X("com.typesafe.akka/akka-actors")("binary-version" -> Set("2.2")),
        X("org.scala-lang/scala-library")("binary-version" -> Set("2.10"))),

    V("org.scala-lang/scala-library")("version" -> Set("2.9.2"),
      "binary-version" -> Set("2.9"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(),
    V("org.scala-lang/scala-library")("version" -> Set("2.9.3"),
      "binary-version" -> Set("2.9"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(),
    V("org.scala-lang/scala-library")("version" -> Set("2.10.2"),
      "binary-version" -> Set("2.10"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(),
    V("org.scala-lang/scala-library")("version" -> Set("2.10.3"),
      "binary-version" -> Set("2.10"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(),

    V("com.typesafe.play/play-slick")("version" -> Set("0.5.0.8"),
      "organization" -> Set("com.typesafe.play"),
      "name" -> Set("play-slick"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(
        X("com.typesafe.play/play")("binary-version" -> Set("2.2")),
        X("org.scala-lang/scala-library")("binary-version" -> Set("2.10"))),

    V("com.typesafe.play/play-slick")("version" -> Set("0.4.0"),
      "organization" -> Set("com.typesafe.play"),
      "name" -> Set("play-slick"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(
        X("com.typesafe.play/play")("binary-version" -> Set("2.1")),
        X("org.scala-lang/scala-library")("binary-version" -> Set("2.10"))),

    V("com.typesafe.akka/akka-actors")("version" -> Set("2.0.5"),
      "organization" -> Set("com.typesafe.akka"),
      "binary-version" -> Set("2.0"),
      "name" -> Set("akka-actors"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(
        X("org.scala-lang/scala-library")("binary-version" -> Set("2.9"))),
    V("com.typesafe.akka/akka-actors")("version" -> Set("2.1.0"),
      "organization" -> Set("com.typesafe.akka"),
      "binary-version" -> Set("2.1"),
      "name" -> Set("akka-actors"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(
        X("org.scala-lang/scala-library")("binary-version" -> Set("2.10"))),
    V("com.typesafe.akka/akka-actors")("version" -> Set("2.1.1"),
      "organization" -> Set("com.typesafe.akka"),
      "binary-version" -> Set("2.1"),
      "name" -> Set("akka-actors"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(
        X("org.scala-lang/scala-library")("binary-version" -> Set("2.10"))),
    V("com.typesafe.akka/akka-actors")("version" -> Set("2.2.0"),
      "organization" -> Set("com.typesafe.akka"),
      "binary-version" -> Set("2.2"),
      "name" -> Set("akka-actors"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(
        X("org.scala-lang/scala-library")("binary-version" -> Set("2.10"))),
    V("com.typesafe.akka/akka-actors")("version" -> Set("2.2.1"),
      "organization" -> Set("com.typesafe.akka"),
      "binary-version" -> Set("2.2"),
      "name" -> Set("akka-actors"),
      "exclusions" -> Set.empty, "overrides" -> Set.empty)(
        X("org.scala-lang/scala-library")("binary-version" -> Set("2.10"))))
}