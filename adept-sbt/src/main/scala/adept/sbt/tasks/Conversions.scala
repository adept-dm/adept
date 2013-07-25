package adept.sbt.tasks

import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.sbt.AdeptKeys._
import adept.sbt.Utils._
import adept.core.models._
import adept.core.Adept
import org.apache.ivy.core.IvyPatternHelper
import akka.util.FiniteDuration

private[tasks] trait Conversions {

  def transformScalaVersion(scalaVersion: String) = {
    if (scalaVersion.startsWith("2.10")) "2.10"
    else scalaVersion
  }

  def nameScalaVersion(name: String, scalaVersion: String) = {
    name + "_" + transformScalaVersion(scalaVersion)
  }

  def majorVersion(version: String) = {
    version.split("\\.").slice(0, 2).mkString(".") //transform to only major versions: 2.10.1 => 2.10
  }

  def adeptArtifact(artifactLocations: Map[String, String], artifact: sbt.Artifact, file: File, sbtPlugin: Boolean, name: String, version: String, organization: String, sbtVersion: String, scalaVersion: String, artifactPatterns: Option[Seq[String]], logger: sbt.Logger)(timeout: FiniteDuration): Artifact = {
    artifactLocations.get(artifact.`type`).map { url =>
      Artifact.fromUrl(new java.net.URL(url), artifact.`type`, artifact.configurations.map(_.name).toSet)(timeout)
    }.getOrElse {
      artifact.url match {
        case Some(url) =>
          Artifact.fromUrl(url, artifact.`type`, artifact.configurations.map(_.name).toSet)(timeout)
        case None =>
          import collection.JavaConverters._
          val currentOrg = (if (!sbtPlugin) nameScalaVersion(organization, scalaVersion) else organization)
          val tokenMap = Map(
            "name" -> artifact.name, //artifact name
            "artifact" -> artifact.name, //artifact name
            "type" -> artifact.`type`,
            "module" -> name, //module name
            "revision" -> version,
            "organization" -> currentOrg,
            "organisation" -> currentOrg,
            "scalaVersion" -> transformScalaVersion(scalaVersion),
            "sbtVersion" -> majorVersion(sbtVersion),
            "classifier" -> artifact.classifier.getOrElse(""),
            "ext" -> artifact.extension).asJava

          val locations = artifactPatterns.map { patterns =>
            patterns.map { pattern =>
              IvyPatternHelper.substituteTokens(pattern, tokenMap)
            }.toSet
          }.getOrElse {
            logger.warn("no locations for " + artifact)
            Set.empty
          }: Set[String]
          Artifact.fromFile(file, artifact.`type`, artifact.configurations.map(_.name).toSet, locations)
      }

    }
  }

  def scalaUniverse(scalaVersion: String) = Universe("scala-version", transformScalaVersion(scalaVersion))
  def sbtUniverse(sbtVersion: String) = Universe("sbt-version", majorVersion(sbtVersion))

  def adeptCoordinates(dep: ModuleID, scalaVersion: String): Coordinates = {
    //TODO: fix crossversions properly 
    dep.crossVersion match {
      case _: CrossVersion.Binary =>
        val name = nameScalaVersion(dep.name, scalaVersion)
        Coordinates(dep.organization, name, dep.revision)
      case _: CrossVersion.Full => throw new Exception("NOT IMPLEMENTED: CrossVersion.Full (sbt plugin)") //TODO: fix...
      case _: CrossVersion.Disabled.type =>
        Coordinates(dep.organization, dep.name, dep.revision)

    }
  }

  def adeptUniverses(dep: ModuleID): Set[Universe] = {
    dep.crossVersion match {
      case _: CrossVersion.Binary =>
        Set.empty
      case _: CrossVersion.Full => throw new Exception("NOT IMPLEMENTED: CrossVersion.Full (sbt plugin)") //TODO: fix...
      case _: CrossVersion.Disabled.type =>
        Set.empty[Universe] ++ dep.extraAttributes.get("e:sbtVersion").map { sbtVersion =>
          sbtUniverse(sbtVersion)
        } ++ dep.extraAttributes.get("e:scalaVersion").map { scalaVersion =>
          scalaUniverse(scalaVersion)
        }
    }
  }

  def adeptExclusion(dep: ModuleID, sbtExclusion: sbt.ExclusionRule): DependencyExclusionRule = {
    if (sbtExclusion.configurations.nonEmpty) throw new Exception("configurations for exclusions are not supported: " + sbtExclusion + " in " + dep)
    else {
      val org = sbtExclusion.organization
      val name = sbtExclusion.name
      DependencyExclusionRule(org, name)
    }
  }

  private val scalaExclusionRule = DependencyExclusionRule("org.scala-lang", "scala-library")

  def adeptDependency(adept: Adept, dep: ModuleID, configurationMapping: String, scalaVersion: String): Option[Dependency] = {
    val coords = adeptCoordinates(dep, scalaVersion)
    val exclusions = dep.exclusions.map(adeptExclusion(dep, _)).toSet + scalaExclusionRule //remove scala, because it is added by sbt
    adept.findModule(coords, uniqueId = None) match { //TODO: change ModuleID to include unique ids as well
      case Right(moduleOpt) => moduleOpt.map { m => Dependency(coords, Some(m.uniqueId), dep.configurations.getOrElse(configurationMapping), isTransitive = dep.isTransitive, force = true, exclusionRules = exclusions) }
      case Left(errorModules) => throw new Exception("Found too many matching modules: " + coords + " " + errorModules.mkString(","))
    }
  }

  def adeptConfiguration(sbtConf: sbt.Configuration): Configuration = {
    val visibility = if (sbtConf.isPublic) Visibility.Public else Visibility.Private
    Configuration(sbtConf.name, Some(sbtConf.description), sbtConf.extendsConfigs.map(_.name).toSet, visibility, None)
  }
}