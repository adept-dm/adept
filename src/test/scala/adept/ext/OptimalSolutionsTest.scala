package adept.ext
import org.scalatest._
import org.scalatest.matchers.MustMatchers
import adept.test.TestDSL._
import adept.test.TestHelpers._
import adept.core.models._
import adept.core.resolution.Resolver
import adept.ext.DefinedVariants
import adept.test.LargeDataSets

class OptimalSolutionsTest extends FunSuite with MustMatchers {

  def getVersion(attributes: Set[Attribute]): Option[String] = {
    attributes.flatMap { attribute =>
      if (attribute.name == "version") {
        if (attribute.values.size > 1) {
          println("WARN: found more than one version in: " + attribute)
        }
        attribute.values.headOption
      } else None
    }.headOption
  }

  test("") {
    val (dependencies, variants) = useTestData(LargeDataSets.basic: _*)

    val result = new Resolver(new DefinedVariants(variants)).resolve(Set(
      //      Dependency("org.scala-lang/scala-library", Set(Constraint("version", Set("2.10.3")))),
      //      Dependency("com.typesafe.akka/akka-actors",  Set(Constraint("version", Set("2.2.0")))),
      //      Dependency(new Id("com.typesafe.play/play"), Set.empty),
      Dependency(new Id("com.typesafe.play/play-slick"), Set.empty) //            Dependency(new Id("com.typesafe.play/play-slick"), Set.empty),
      //            Dependency(new Id("com.typesafe.play/play"), Set.empty)
      ))

    result match {
      case underconstrainedResult: UnderconstrainedResult =>
        //println(underconstrainedResult.optimalStates)
        println(underconstrainedResult.optimalStates.map(_.implicitVariants).mkString("\n\n"))

        
        //find highest version for each id and the other attributes
        //remove states which variants with the same attributes on the implicits variants, and a lower version (do not remove if version is not found)
        
        var a = Map.empty[Id, Set[Map[Set[Attribute], Version]]] //
        
        underconstrainedResult.optimalStates.foreach { state =>
          state.implicitVariants.foreach {
            case (id, variant) =>
//              if (a.get(id).isDefined) {
//                a.get(id)
//                getVersion(variant.attributes).foreach{ version =>
//                  if (Version(version) > )
//                }
//              }
//                
              
          }
        }

      case _ => assert(false, "result was not under-constrained: " + result)
    }
  }
}