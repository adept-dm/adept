package adept.ext

import adept.core.resolution.Resolver
import adept.core.models._
import adept.core.models.internal._
import java.security.MessageDigest

case class Query(exprs: (String, String)*)

object Query {
  def matches(variant: Variant, query: Query): Boolean = {
    val attributes = variant.attributes
    val constraints = query.exprs
    val zipped = query.exprs.map {
      case expr @ (exprName, _) =>
        expr -> attributes.filter(attribute => attribute.name == exprName)
    }
    zipped.forall {
      case ((exprName, exprValue), matchingAttributes) =>
        matchingAttributes.exists { attribute =>
          exprValue match {
            case "*" => true
            case _ =>
              attribute.values.contains(exprValue)
          }
        }
    }
  }
}

object Hash {
  private lazy val md: ThreadLocal[MessageDigest] = new ThreadLocal[MessageDigest] { //make message digest thread-"safe"
    override def initialValue() = {
      MessageDigest.getInstance("SHA-256")
    }
  }

  private def encode(bytes: Array[Byte]) = {
    md.get().digest(bytes).map(b => "%02X" format b).mkString.toLowerCase
  }

  private def updateWithConstraint(constraint: Constraint, currentMd: MessageDigest) = {
    currentMd.update(constraint.name.getBytes)
    constraint.values.foreach { value =>
      currentMd.update(value.getBytes)
    }
  }

  private def updateWithArtifact(artifact: Artifact, currentMd: MessageDigest) = {
    currentMd.update(artifact.hash.getBytes)
    artifact.attributes.foreach(updateWithAttribute(_, currentMd))
  }

  private def updateWithAttribute(attribute: Attribute, currentMd: MessageDigest) = {
    currentMd.update(attribute.name.getBytes)
    attribute.values.foreach { value =>
      currentMd.update(value.getBytes)
    }
  }

  private def updateWithDependency(dependency: Dependency, currentMd: MessageDigest) = {
    currentMd.update(dependency.id.getBytes)
    dependency.constraints.foreach(updateWithConstraint(_, currentMd))
  }

  def calculate(dependencies: Set[Dependency]): String = {
    val currentMd = md.get()
    currentMd.reset()
    try {
      dependencies.foreach(updateWithDependency(_, currentMd))

      currentMd.digest().map(b => "%02X" format b).mkString.toLowerCase
    } finally {
      currentMd.reset()
    }
  }

  def calculate(variant: Variant): String = {
    val currentMd = md.get()
    currentMd.reset()
    try {
      currentMd.update(variant.id.getBytes)
      variant.dependencies.foreach(updateWithDependency(_, currentMd))
      variant.artifacts.foreach(updateWithArtifact(_, currentMd))
      variant.attributes.foreach(updateWithAttribute(_, currentMd))

      currentMd.digest().map(b => "%02X" format b).mkString.toLowerCase
    } finally {
      currentMd.reset()
    }
  }
}

//TODO: remove unnecessary fields
case class ReplaceResult(dependencies: Set[Dependency], newVariants: Set[Variant], attributes: Map[String, Set[Attribute]],
  includedVariants: Map[String, Variant], graph: Set[Node])

object Extensions {

  def mergeAttributes(allAttributes: Set[Attribute]*): Set[Attribute] = {
    allAttributes.flatten.groupBy(_.name).map {
      case (name, attrs) =>
        Attribute(name, attrs.toSet.flatMap((_: Attribute).values))
    }(collection.breakOut)
  }

  def mergeConstraints(allConstraints: Set[Constraint]*): Set[Constraint] = {
    allConstraints.flatten.groupBy(_.name).map {
      case (name, attrs) =>
        Constraint(name, attrs.toSet.flatMap((_: Constraint).values))
    }(collection.breakOut)
  }

  val ExclusionAttributeName = "exclusions"

  /** excluded attributes looks like this: "exclusions": [ "foo/bar:123aef" ] */
  def excludedAttribute(variant: Variant): Attribute = {
    Attribute(ExclusionAttributeName, Set(variant.id + ":" + Hash.calculate(variant)))
  }

  def exclude(baseDependencies: Set[Dependency], graph: Set[Node], variants: Map[String, Variant], query: Query): ReplaceResult = {
    replaceNode(baseDependencies, graph, variants, query) { (matchingDependencies, variant) =>
      val excludedAttributes = matchingDependencies.map(dependency => excludedAttribute(variants(dependency.id)))
      val newVariant = variant.copy(dependencies = variant.dependencies.filter(d => !matchingDependencies.contains(d)),
        attributes = mergeAttributes(variant.attributes, excludedAttributes))
      excludedAttributes -> newVariant
    }
  }

  val OverriddenAttributeName = "overrides"

  def overriddenAttribute(variant: Variant, newDependencies: Set[Dependency]): Attribute = {
    val newDepsHash = Hash.calculate(newDependencies)
    val oldDepsHash = Hash.calculate(variant.dependencies)
    
    Attribute(OverriddenAttributeName, Set(variant.id + ":" + oldDepsHash + ":" + newDepsHash))
  }

  def overrides(baseDependencies: Set[Dependency], graph: Set[Node], variants: Map[String, Variant], query: Query, replacements: Map[String, Set[Attribute]]): ReplaceResult = {
    replaceNode(baseDependencies, graph, variants, query) { (matchingDependencies, variant) =>
      val overrideDependencies = variant.dependencies.map { dependency =>
        replacements.get(dependency.id) match {
          case Some(attrs) =>
            val names = attrs.map(_.name)
            val constraints = dependency.constraints.filter(constraint => !names(constraint.name)) ++ attrs.map(_.toConstraint)
            dependency.copy(constraints = constraints)
          case None => dependency
        }
      }
      val overriddenAttributes = matchingDependencies.map(dependency => overriddenAttribute(variants(dependency.id), overrideDependencies))
      
      val newVariant = variant.copy(dependencies = overrideDependencies,
        attributes = mergeAttributes(variant.attributes, overriddenAttributes))
      overriddenAttributes -> newVariant
    }
  }

  private def replaceNode(replacementDependencies: Set[Dependency], graph: Set[Node], variants: Map[String, Variant], query: Query)(replaceFun: (Set[Dependency], Variant) => (Set[Attribute], Variant)): ReplaceResult = {
    //FIXME: vars...
    var includedVariants = Map.empty[String, Variant]
    var newVariants = Set.empty[Variant]
    var attributes = Map.empty[String, Set[Attribute]]
    var dependencies = Map.empty[String, Dependency]
    var newDependencies = Set.empty[Dependency]

    val dependencyMap = replacementDependencies.map { dependency =>
      dependency.id -> dependency
    }.toMap

    def transitiveReplace(nodes: Set[Node]): Set[Node] = { //TODO: tailrec??
      nodes.map { node =>
        val variant = variants(node.id)
        //find matching dependencies based on query
        val matchingDependencies = variant.dependencies.filter { dependency =>
          Query.matches(variants(dependency.id), query)
        }

        //find new variant if it is to be replaced or use old one
        val includedVariant = if (matchingDependencies.nonEmpty) {
          val (replacedAttributes, newVariant) = replaceFun(matchingDependencies, variant)

          newVariants += newVariant
          attributes += node.id -> replacedAttributes

          val newConstraints = replacedAttributes.map(_.toConstraint)
          dependencyMap.get(node.id) match { //found a dependency we have already, so we must add a attributes to it
            case Some(declaredDependency) =>
              dependencies += node.id -> declaredDependency.copy(constraints = mergeConstraints(declaredDependency.constraints, newConstraints))
            case None => newDependencies += Dependency(node.id, newConstraints) //a new dependency with excluded constraints
          }
          newVariant
        } else variant

        includedVariants += node.id -> includedVariant

        val excludedIds = matchingDependencies.map(_.id)

        val children = transitiveReplace(node.children.filter { child => //remove all excluded children
          !excludedIds.contains(child.id)
        })
        node.copy(id = node.id, children = children)
      }
    }

    val newGraph = transitiveReplace(graph)
    val replacedDependencies = replacementDependencies.map { replacementDependency =>
      dependencies.getOrElse(replacementDependency.id, replacementDependency)
    }
    ReplaceResult(replacedDependencies ++ newDependencies, newVariants, attributes, includedVariants, newGraph)
  }
}
