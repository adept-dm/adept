package adept.core.models

case class DependencyExclusionRule(org: String, name: String) {
  override def toString = {
    org + ":" + name
  }

  def matches(dependency: Dependency): Boolean = { //TODO: move logic out of model
    (org, name) match {
      case ("*", "*") => true
      case ("*", _) if dependency.coordinates.name == name => true
      case (_, _) if dependency.coordinates.org == org && dependency.coordinates.name == name => true
      case (_, "*") if dependency.coordinates.org == org => true
      case (_, _) => false
    }
  }
}

object DependencyExclusionRule {
  import org.json4s._
  import adept.utils.JsonHelpers._
  import org.json4s.JsonDSL._

  def readDependencyExclusionRule(json: JValue): Either[String, DependencyExclusionRule] = {
    for {
      org <- (eitherOf[String](json) \ "organization").right
      name <- (eitherOf[String](json) \ "name").right
    } yield {
      DependencyExclusionRule(org, name)
    }
  }

  def toJson(exclusionRule: DependencyExclusionRule): JValue = {
    ("organization" -> exclusionRule.org) ~
      ("name" -> exclusionRule.name)
  }

  def readDependencyExclusionRules(json: JValue): Either[String, Set[DependencyExclusionRule]] = {
    readSet(json) { f =>
      f.map(readDependencyExclusionRule)
    }
  }
}
