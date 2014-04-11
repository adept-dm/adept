package adept.ivy

import org.apache.ivy.core.module.descriptor.ExcludeRule
import adept.resolution.models.Variant
import org.apache.ivy.core.report.ResolveReport
import org.apache.ivy.util.AbstractMessageLogger
import org.apache.ivy.util.Message
import org.apache.ivy.Ivy
import org.apache.ivy.core.resolve.IvyNode
import org.apache.ivy.core.IvyContext
import java.io.File
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.core.module.id.ModuleId
import adept.resolution.models.Id
import adept.repository.models.RepositoryName
import adept.ext.Version
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.module.descriptor.DependencyDescriptor
import adept.logging.Logging
import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import org.apache.ivy.core.cache.ResolutionCacheManager
import org.apache.ivy.core.search.OrganisationEntry
import org.apache.ivy.core.search.ModuleEntry
import org.apache.ivy.plugins.resolver.URLResolver
import org.apache.ivy.core.search.RevisionEntry
import org.eclipse.jgit.lib.ProgressMonitor

private[adept] object IvyUtils extends Logging {
  import IvyConstants._
  import collection.JavaConverters._

  /** As in sbt */
  private[ivy] def cleanModule(mrid: ModuleRevisionId, resolveId: String, manager: ResolutionCacheManager) {
    val files =
      Option(manager.getResolvedIvyPropertiesInCache(mrid)).toList :::
        Option(manager.getResolvedIvyFileInCache(mrid)).toList :::
        Option(manager.getResolvedIvyPropertiesInCache(mrid)).toList :::
        Option(manager.getConfigurationResolveReportsInCache(resolveId)).toList.flatten
    import scala.reflect.io.Directory
    files.foreach { file =>
      (new Directory(file)).deleteRecursively() //TODO: I hope this works on files and on directories? Perhaps use something else? 
    }
  }

  def getExcludeRules(parentNode: IvyNode, ivyNode: IvyNode) = {
    for { //handle nulls
      parentNode <- Option(parentNode).toSet[IvyNode]
      currentIvyNode <- Option(ivyNode).toSet[IvyNode]
      dependencyDescriptor <- Option(currentIvyNode.getDependencyDescriptor(parentNode)).toSet[DependencyDescriptor]
      excludeRule <- {
        if (dependencyDescriptor.getAllIncludeRules().nonEmpty) {
          logger.warn("in: " + parentNode + " there is a dependency:" + currentIvyNode + " which has inlcude rules: " + dependencyDescriptor.getAllIncludeRules().toList + " which are not supported") //TODO: add support
        }
        dependencyDescriptor.getAllExcludeRules()
      }
    } yield {
      val moduleId = excludeRule.getId.getModuleId
      moduleId.getOrganisation -> moduleId.getName
    }
  }

  def getParentNode(resolveReport: ResolveReport) = {
    resolveReport.getDependencies().asScala.map { case i: IvyNode => i }.head //Feels a bit scary?
  }

  def load(path: Option[String] = None, ivyLogger: AbstractMessageLogger = errorIvyLogger): Ivy = {
    //setting up logging
    Message.setDefaultLogger(ivyLogger)
    val ivy = IvyContext.getContext.getIvy
    val loadedIvy = path.map { path =>
      val ivySettings = new File(path)
      if (!ivySettings.isFile) {
        throw AdeptIvyException(ivySettings + " is not a file")
      } else {
        ivy.configure(ivySettings)
        ivy
      }
    }.getOrElse {
      ivy.configureDefault()
      ivy
    }

    val settings = loadedIvy.getSettings()
    //ivyRoot.foreach(settings.setDefaultIvyUserDir) //FIXME: TODO I do not understand why this does not WORK?!?! Perhaps I didn't well enough?
    loadedIvy.setSettings(settings)
    loadedIvy
  }

  val ConfigRegex = "^(.*)/config/(.*?)$".r //useful to extract configurations

  def resolveOptions(confs: String*) = {
    val resolveOptions = new ResolveOptions()
    if (confs.nonEmpty) resolveOptions.setConfs(confs.toArray)
    resolveOptions.setCheckIfChanged(true)
    resolveOptions.setRefresh(true)
    resolveOptions.setDownload(true)
    resolveOptions.setOutputReport(false) //TODO: to true?
    resolveOptions
  }

  private def getUrlResolvers(ivy: Ivy) = {
    val settings = ivy.getSettings
    val urlResolvers = settings.getResolverNames().asScala.toList.flatMap {
      case name: String => //names must be strings (at least I think so...)
        settings.getResolver(name) match {
          case urlResolver: URLResolver => Some(urlResolver)
          case _ => None
        }
    }
    urlResolvers
  }

  def list(ivy: Ivy, org: String, progress: ProgressMonitor): Set[(String, String, String)] = {
    progress.beginTask("Listing " + org, ProgressMonitor.UNKNOWN)
    val all = getUrlResolvers(ivy).flatMap { dependencyResolver =>
      progress.update(1)
      dependencyResolver.listModules(new OrganisationEntry(dependencyResolver, org)).flatMap { moduleEntry =>
        progress.update(1)
        dependencyResolver.listRevisions(moduleEntry).map { revisionEntry =>
          progress.update(1)
          (revisionEntry.getOrganisation, revisionEntry.getModule, revisionEntry.getRevision)
        }
      }
    }.toSet
    progress.endTask()
    all
  }

  def list(ivy: Ivy, org: String, module: String, progress: ProgressMonitor): Set[(String, String, String)] = {
    progress.beginTask("Listing " + org + "#" + module, ProgressMonitor.UNKNOWN)
    val all = getUrlResolvers(ivy).flatMap { dependencyResolver =>
      val orgEntry = new OrganisationEntry(dependencyResolver, org)
      val moduleEntry = new ModuleEntry(orgEntry, module)
      progress.update(1)
      dependencyResolver.listRevisions(moduleEntry).map { revisionEntry =>
        progress.update(1)
        (revisionEntry.getOrganisation, revisionEntry.getModule, revisionEntry.getRevision)
      }
    }.toSet
    progress.endTask()
    all
  }

  def ivyIdAsId(moduleId: ModuleId): Id = {
    Id(moduleId.getOrganisation + Id.Sep + moduleId.getName)
  }

  def withConfiguration(id: Id, confName: String): Id = {
    Id(id.value + Id.Sep + IdConfig + Id.Sep + confName)
  }

  def ivyIdAsId(moduleId: ModuleId, confName: String): Id = {
    assert(!confName.contains(Id.Sep))
    withConfiguration(ivyIdAsId(moduleId), confName)
  }

  def ivyIdAsRepositoryName(moduleId: ModuleId): RepositoryName = {
    RepositoryName(moduleId.getOrganisation)
  }

  def ivyIdAsVersion(mrid: ModuleRevisionId): Version = {
    Version(mrid.getRevision)
  }

  def matchesExcludeRule(excludeOrg: String, excludeName: String, variant: Variant): Boolean = {
    val res = variant.attribute(IvyNameAttribute).values == Set(excludeName) &&
      variant.attribute(IvyOrgAttribute).values == Set(excludeOrg)
//    if (!res) println(moduleId + " VS name: " + variant.attribute(IvyNameAttribute).values + " AND org: " + variant.attribute(IvyOrgAttribute).values)
    res
  }
}