package adept.ivy

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.resolution.models._
import adept.ext.Version
import adept.repository.models.VariantHash
import adept.repository.models.RepositoryName

class IvyInserterTest extends FunSuite with Matchers {
  import adept.test.ResolverUtils._

  test("Basics: verify that IvyInserter can add ivy results") {
    val variants = Set(
      Variant("akka-actor/config/runtime", Set(version -> Set("2.2.0")),
        requirements = Set(
          "akka-actor" -> Set[Constraint]("configuration" -> Set("1234")),
          "config/config/runtime" -> Set.empty[Constraint],
          "scala-library/config/runtime" -> Set.empty[Constraint])),
      Variant("akka-actor/config/compile", Set(version -> Set("2.2.0")),
        requirements = Set(
          "akka-actor" -> Set[Constraint]("configuration" -> Set("1234")),
          "config/config/compile" -> Set.empty[Constraint],
          "scala-library/config/compile" -> Set.empty[Constraint])))

    Map.empty[(RepositoryName, VariantHash), Set[(RepositoryName, Id, Version)]] //
  }
}