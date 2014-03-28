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
      excludeRule
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

  def resolveOptions(confs: String*) = {
    val resolveOptions = new ResolveOptions()
    if (confs.nonEmpty) resolveOptions.setConfs(confs.toArray)
    resolveOptions.setCheckIfChanged(true)
    resolveOptions.setRefresh(true)
    resolveOptions.setDownload(true)
    resolveOptions.setOutputReport(false) //TODO: to true?
    resolveOptions
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

  def matchesExcludeRule(excludeRule: ExcludeRule, variant: Variant): Boolean = {
    val moduleId = excludeRule.getId.getModuleId
    variant.attribute(IvyNameAttribute).values == Set(moduleId.getName()) &&
      variant.attribute(IvyOrgAttribute).values == Set(moduleId.getOrganisation())
  }
}