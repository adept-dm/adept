package adept.models

import java.io.File
import org.json4s._
import org.json4s.Extraction._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import adept.utils.EitherUtils
import java.io.StringWriter
import java.io.PrintWriter
import adept.utils.Logging

case class Module(
  coordinates: Coordinates,
  artifacts: Set[Artifact],
  configurations: Set[Configuration],
  attributes: Map[String, Seq[String]],
  dependencies: Set[Dependency]) {
  lazy val hash = Hash.mix(artifacts.map(_.hash).toSeq)
}

object Module {

  def readSameCoordinates(json: JValue): Either[String, Seq[Module]] = {
    import reads._
    val res = for {
      coords <- readCoords(json).right
      modules <- readModulesWithUsingCoords(coords, json).right
    } yield {
      modules
    }
    res
  }

  def writeJsonForSameCoords(coords: Coordinates, modules: Seq[Module]): JValue = {
    import writes._
    coordsToJson(coords) ~ asJObject(List[JField](
      ("modules" -> modules.map { module =>
        if (coords != module.coordinates) throw new Exception("expected module: " + module + " to have coords: " + coords + ". modules: " + modules)
        noCoordsModuleToJson(module)
      })).map(ifNonEmpty): _*)
  }

  private object reads extends Logging {
      
    trait JsonTransformer[T] {
      def eitherOf(json: JValue, s: String): Either[String, T]
    }
    
    def issueError[A](json: JValue, s: String, expected: String, got: JValue): Either[String, A] = {
      if (true || logger.isDebugEnabled) {
        val e = new Exception()
        val w = new StringWriter()
        e.printStackTrace(new PrintWriter(w))
        logger.debug("printing stacktrace for debug: " + w.getBuffer().toString)
        println(w.getBuffer().toString)
        logger.debug("pure json was: " + json)
        println("pure json was: " + json)
      }
      Left("expected a "+expected+" but found:" +got + " for " + s + " in:" + pretty(render(json)))
    }
    
    implicit val jstringTransformer = new JsonTransformer[String]{
      override def eitherOf(json: JValue, s: String): Either[String, String] = {
        (json \ s) match {
          case JString(v) => Right(v)
          case a => issueError(json, s, "JString", a)
        }
      }
    }
    
    
    def eitherOf[T](json: JValue)(implicit t: JsonTransformer[T]) = new {
      def \(s: String): Either[String, T] = {
        t.eitherOf(json, s)
      }
    }

    def readDependency(json: JValue): Either[String, Dependency] = {
      for {
        hash <- (eitherOf[String](json) \ "hash").right
        configuration <- (eitherOf[String](json) \ "configuration").right
        coords <- readCoords(json).right
      } yield {
        Dependency(coords, Hash(hash), configuration)
      }
    }
    
    def getOptionalValue[A : JsonTransformer](json: JValue, name: String): Either[String, Option[A]]  = {
      val maybeValue = (json \ name).toOption //is Some if value exists
      
       //if value exists it should be of the expected type A, and wrapped in Option
      val mappedValue = maybeValue.map(_ => (eitherOf[A](json) \ name).right.map(a => Some(a)))
      
      mappedValue.getOrElse(Right(None)) //if no value is fine this is Ok
    }
    
    def getOptionalStrings(json: JValue, name: String): Either[String, Set[String]] = {
      (json \ name).toOption.map{ foundJson =>
          readSet(foundJson)(array => array.map{  stringJson =>
           stringJson match {
            case JString(v) => Right(v)
            case a => issueError(json, name, "JString", a)
           }
        })
      }.getOrElse{
        Right(Set.empty)
      }
    }
    
    def readConfiguration(json: JValue): Either[String, Configuration] = {
      val maybeExtendsFrom = getOptionalStrings(json, "extends")
      val eithers = for {
        name <- (eitherOf[String](json) \ "name").right
        visibilityString <- (eitherOf[String](json) \ "visibility").right
        visibility <- Visibility.values.find(_.toString.toLowerCase == visibilityString).toRight("Could not find visibility: " + visibilityString).right
        extendsFrom <- maybeExtendsFrom.right
      } yield {
         for {
           deprectated <- getOptionalValue[String](json, "deprecated").right
           description <- getOptionalValue[String](json, "description").right
         } yield {
           Configuration(name, description, extendsFrom, visibility, deprectated)
         }
      }
      eithers.joinRight
    }
    
    def readArtifact(json: JValue): Either[String, Artifact] = {
      val maybeLocations = getOptionalStrings(json, "locations")

      val maybeConfigurations = getOptionalStrings(json, "configurations")

      for {
        hash <- (eitherOf[String](json) \ "hash").right
        artifactType <- (eitherOf[String](json) \ "artifact-type").right
        locations <- maybeLocations.right
        configurations <- maybeConfigurations.right
      } yield {
        Artifact(Hash(hash), artifactType, configurations, locations)
      }
    }
    
    def readDependencies(json: JValue): Either[String, Set[Dependency]] = {
      readSet(json){ f =>
        f.map(readDependency)
      }      
    }
    
    def readConfigurations(json: JValue): Either[String, Set[Configuration]] = {
      readSet(json){ f =>
        f.map(readConfiguration)
      }
    }
    
    def readArtifacts(json: JValue): Either[String, Set[Artifact]] = {
      readSet(json){ f => 
        f.map(readArtifact)
      }
    }
    
    def readSet[A](json: JValue)(f: List[JValue] => Seq[Either[String, A]]): Either[String, Set[A]] = {
      readSeq(json)(f).right.map(_.toSet)
    }
    
    def readSeq[A](json: JValue)(f: List[JValue] => Seq[Either[String, A]]): Either[String, Seq[A]] = {
      json match {
        case JArray(list) => 
          EitherUtils.reduce(f(list))
        case somethingElse => {
          issueError(json, "JArray", "an array", somethingElse)
        }
      }
    }
    
    def readModuleWithUsingCoords(coords: Coordinates, json: JValue): Either[String, Module] = {
      val maybeArtifacts = readArtifacts((json \ "artifacts"))
      
      val maybeConfigurations = {
        (json \ "configurations").toOption.map(readConfigurations)
      }.getOrElse{
        Right(Set.empty[Configuration])
      }
      
      val maybeDependencies = {
        (json \ "dependencies").toOption.map(readDependencies)
      }.getOrElse{
        Right(Set.empty[Dependency])
      }

      val attributes: Map[String, Seq[String]] = {
        implicit val format = org.json4s.DefaultFormats
        (json \ "attributes").extractOpt[Map[String, Seq[String]]]
      }.getOrElse{
        Map.empty
      }

      for {
        artifacts <- maybeArtifacts.right
        configurations <- maybeConfigurations.right
        dependencies <- maybeDependencies.right
      } yield {
        Module(coords, artifacts, configurations, attributes, dependencies)
      }
    }
    
    def readModulesWithUsingCoords(coords: Coordinates, json: JValue): Either[String, Seq[Module]] = {
      readSeq(json \ "modules"){ f => 
        f.map(j => readModuleWithUsingCoords(coords, j))
      }
    }
    
    def readCoords(json: JValue): Either[String, Coordinates] = {
      for {
        org <- (eitherOf[String](json) \ "organization").right
        name <- (eitherOf[String](json) \ "name").right
        version <- (eitherOf[String](json) \ "version").right
      } yield {
        Coordinates(org, name, version)
      }
    }
  }

  private object writes {

    def ifNonEmpty(f: JField): Option[JField] = {
      val (key, value) = f
      value match {
        case JObject(obj) if obj.isEmpty => None
        case JArray(arr) if arr.isEmpty => None
        case _ => Some(f)
      }
    }

    def asJObject(l: Option[JField]*): JObject = {
      JObject(List[Option[JField]](l: _*).flatten)
    }

    def artifactToJson(artifact: Artifact): JValue = {
      asJObject(List[JField](
        ("hash" -> artifact.hash.value),
        ("artifact-type" -> artifact.artifactType),
        ("configurations" -> artifact.configurations),
        ("locations" -> artifact.locations)).map(ifNonEmpty): _*)
    }

    def coordsToJson(coords: Coordinates) = {
      ("name" -> coords.name) ~
        ("organization" -> coords.org) ~
        ("version" -> coords.version)

    }

    def dependencyToJson(dep: Dependency): JValue = {
      ("hash" -> dep.hash.value) ~
        ("configuration" -> dep.configuration) ~
        coordsToJson(dep.coords)
    }

    def configurationToJson(config: Configuration): JValue = {
      asJObject(List[JField](
        ("name" -> config.name),
        ("description" -> config.description),
        ("visibility" -> config.visibility.toString.toLowerCase),
        ("extends" -> config.extendsFrom),
        ("deprecated" -> config.deprecated)).map(ifNonEmpty): _*)
    }

    def noCoordsModuleToJson(module: Module): JObject = {
      asJObject(List[JField](
        ("hash" -> module.hash.value),
        ("artifacts" -> module.artifacts.map(artifactToJson)),
        ("attributes" -> module.attributes),
        ("dependencies" -> module.dependencies.map(dependencyToJson)),
        ("configurations" -> module.configurations.map(configurationToJson))).map(ifNonEmpty): _*)
    }
  }

}