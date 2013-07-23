package adept.ivy.utils

import adept.core.models._
import java.io.{ File => jFile }
import org.apache.ivy._
import org.apache.ivy.core._
import org.apache.ivy.core.resolve._
import org.apache.ivy.core.report.ResolveReport
import scala.util._
import java.io.File
import adept.core.Adept
import org.apache.ivy.util.Message
import org.apache.ivy.util.DefaultMessageLogger
import adept.utils.Logging
import collection.JavaConverters._
import org.apache.ivy.core.report.ResolveReport
import org.apache.ivy.core.module.descriptor.{ Artifact => IvyArtifact }
import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }
import java.io.{File => jFile}
import org.apache.ivy.core.module.descriptor.{Artifact => IvyArtifact}
import org.apache.ivy.core.module.descriptor.{Configuration => IvyConfiguration}
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.plugins.resolver.DependencyResolver
import org.apache.ivy.core.search.ModuleEntry
import org.apache.ivy.core.search.OrganisationEntry

object IvyHelpers extends Logging {

  def load(path: Option[String] = None, logLevel: Int = Message.MSG_ERR): Either[String, Ivy] = {
    //setting up logging
    val ivyLogger = new DefaultMessageLogger(logLevel)
    //ivyLogger.setShowProgress(false);
    //Message.setDefaultLogger(ivyLogger)
    val ivy = IvyContext.getContext.getIvy
    val res = path.map { path =>
      val ivySettings = new jFile(path)
      if (!ivySettings.isFile) {
        Left(ivySettings + " is not a file")
      } else {
        ivy.configure(ivySettings)
        Right(ivy)
      }
    }.getOrElse {
      ivy.configureDefault()
      Right(ivy)
    }
    res.right.map { ivy =>
      val settings = ivy.getSettings()
      //ivyRoot.foreach(settings.setDefaultIvyUserDir) //FIXME: TODO this does not WORK?!?!
      ivy.setSettings(settings)
      ivy
    }
  }

  def resolveOptions(confs: String*) = {
    val resolveOptions = new ResolveOptions()
    if (confs.nonEmpty) resolveOptions.setConfs(confs.toArray)
    resolveOptions.setCheckIfChanged(true)
    resolveOptions.setRefresh(true)
    resolveOptions.setDownload(true)
    resolveOptions.setOutputReport(false) //TODO: to true?
    resolveOptions
  }

  def confString(modConf: String, depConfs: Array[String]) = {
    modConf + "->" + depConfs.mkString(",")
  }
  val changing = true

  def parent(report: ResolveReport): IvyNode = {
    report.getDependencies().asScala.map { case i: IvyNode => i }.head
  }

  def mustBeAvailable(conf: Configuration): Boolean = {
    (conf.name.toLowerCase match {
      case "optional" => false
      case "system" => false
      case "provided" => false
      case _ => true
    }) && (
      conf.visibility == Visibility.Public)
  }

  def resolveDynamicVersion(initCoords: Coordinates, ivy: Ivy): Coordinates = {
    val versionMatcher = ivy.getResolveEngine().getSettings().getVersionMatcher()
    val isDynamic = versionMatcher.isDynamic(ModuleRevisionId.newInstance(initCoords.org, initCoords.name, initCoords.version))

    if (isDynamic) {
      val all = ivy.getSettings().getResolvers.asScala.flatMap {
        case resolver: DependencyResolver =>
          resolver.listRevisions(new ModuleEntry(new OrganisationEntry(resolver, initCoords.org), initCoords.name)).map {
            _.getRevision
          }.filter { version =>
            versionMatcher.accept(ModuleRevisionId.newInstance(initCoords.org, initCoords.name, initCoords.version),
              ModuleRevisionId.newInstance(initCoords.org, initCoords.name, version))
          }
        case other =>
          logger.warn("skipping dynamic version this resolver on: " + initCoords + " because found " + other.getClass + " and not DependencyResolver")
          Set.empty
      }

      val latestVersion = all.reduce { (last, current) =>
        val lastVersion = ModuleRevisionId.newInstance(initCoords.org, initCoords.name, last)
        val currentVersion = ModuleRevisionId.newInstance(initCoords.org, initCoords.name, current)

        if (versionMatcher.accept(lastVersion, currentVersion)) {
          lastVersion.getRevision()
        } else currentVersion.getRevision()
      }

      initCoords.copy(version = latestVersion)
    } else {
      initCoords
    }
  }


}
