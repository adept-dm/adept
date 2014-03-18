package adept.ivy

import adept.ext.AttributeDefaults
import org.apache.ivy.Ivy
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.core.module.descriptor.{ Artifact => IvyArtifact }
import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }
import org.apache.ivy.util.Message
import org.apache.ivy.util.DefaultMessageLogger
import org.apache.ivy.core.IvyContext
import java.io.File
import org.apache.ivy.core.resolve.IvyNode
import collection.JavaConverters._
import org.apache.ivy.core.module.descriptor.Configuration.Visibility
import org.apache.ivy.core.report.ResolveReport
import org.apache.ivy.util.AbstractMessageLogger
import adept.ext.Version
import adept.logging.Logging
import adept.artifact.models._
import adept.repository.models._
import adept.resolution.models._
import adept.utils.Hasher
import java.io.FileInputStream
import adept.repository.serialization.VariantMetadata
import adept.repository.GitRepository
import adept.ext.VersionOrder
import adept.repository.serialization.ResolutionResultsMetadata
import org.eclipse.jgit.lib.ProgressMonitor
import org.apache.ivy.plugins.matcher.MapMatcher
import org.apache.ivy.core.module.descriptor.OverrideDependencyDescriptorMediator
import org.apache.ivy.plugins.matcher.PatternMatcher
import org.apache.ivy.core.IvyPatternHelper
import org.apache.ivy.core.module.descriptor.{ ModuleDescriptor, DependencyDescriptor }
import org.apache.ivy.core.cache.ResolutionCacheManager
import org.apache.ivy.core.module.id.ModuleId
import org.apache.ivy.core.module.descriptor.ExcludeRule

case class AdeptIvyResolveException(msg: String) extends Exception(msg)
case class AdeptIvyException(msg: String) extends Exception(msg)

object IvyHelper extends Logging {
  import AttributeDefaults.{ NameAttribute, OrgAttribute, VersionAttribute }

  lazy val errorIvyLogger = new DefaultMessageLogger(Message.MSG_ERR) {
    var i = 0

    override def doProgress(): Unit = {
      val indicator = if (i == 0) "-"
      else if (i == 1) "/"
      else if (i == 2) "-"
      else if (i == 3) "\\"
      else if (i == 4) "|"
      else {
        i = 0
        "/"
      }
      i = i + 1
      //System.out.print("\r" * 80 + " " * 80 + "\r" * 80)
      System.out.print(indicator + "\r")
    }

    override def doEndProgress(ivyMsg: String): Unit = {
      //pass
    }
  }
  lazy val warnIvyLogger = new DefaultMessageLogger(Message.MSG_WARN)
  lazy val infoIvyLogger = new DefaultMessageLogger(Message.MSG_INFO)
  lazy val debugIvyLogger = new DefaultMessageLogger(Message.MSG_DEBUG)

  private def matchesExcludeRule(exclusionRule: ExcludeRule, id: Id, repository: RepositoryName): Boolean = {
    repository.value == exclusionRule.getId().getModuleId().getOrganisation() &&
      (id.value.startsWith(exclusionRule.getId().getModuleId().getName()) || //TODO: this is not completely correct! Must think more about how this should work
        id.value == exclusionRule.getId().getModuleId().getName())
  }

  def insert(baseDir: File, results: Set[IvyImportResult], progress: ProgressMonitor): Set[ResolutionResult] = {
//    progress.beginTask("Applying exclusion(s)", results.size * 2)
//    val excluded = results.flatMap { result =>
//      for {
//        otherResult <- {
//          progress.update(1)
//          results
//        }
//        otherExclusionRule <- otherResult.exclusionRules
//        if (matchesExcludeRule(otherExclusionRule, result.variant.id, result.repository))
//      } yield {
//        logger.debug("Excluding: " + result + " because of exclusion: " + otherExclusionRule + " in " + otherResult)
//        ((result.repository, result.variant.id))
//      }
//    }
//
//    val includedResults = results.flatMap { result =>
//      val isExcluded = excluded(result.repository, result.variant.id)
//      progress.update(1)
//      if (!isExcluded) {
//        val exclusions = for {
//          exclusionRule <- result.exclusionRules
//          (repository, id) <- excluded
//          if matchesExcludeRule(exclusionRule, id, repository)
//        } yield {
//          id
//        }
//        val variant = result.variant
//        Some(result.copy(variant = variant))
//      } else {
//        None
//      }
//    }
//
//    progress.endTask()
//
//    val grouped = includedResults.groupBy(_.repository) //grouping to avoid multiple parallel operations on a repo
//
//    progress.beginTask("Writing Ivy results to repo(s)", grouped.size)
//    grouped.par.foreach { //NOTICE .par TODO: replace with something more optimized for IO not for CPU
//      case (_, results) =>
//        results.foreach { result =>
//          val variant = result.variant
//          val id = variant.id
//
//          val repository = new GitRepository(baseDir, result.repository)
//          if (!repository.exists) repository.init()
//          val variantMetadata = VariantMetadata.fromVariant(variant)
//          //
//          //          println("wrote"+id + " in "+repository.name)
//          repository.add(variantMetadata.write(id, repository))
//          val commit = repository.commit("Ivy Import of " + variant.id)
//          repository.add(VersionOrder.useDefaultVersionOrder(id, repository, commit))
//          repository.commit("Ordered Ivy Import of " + variant.id)
//        }
//        progress.update(1)
//    }
//    progress.endTask()
//    progress.beginTask("Converting Ivy version in repo(s)", grouped.size)
//    val all = Set() ++ grouped.par.flatMap { //NOTICE .par TODO: same as above (IO vs CPU)
//      case (_, results) =>
//        val completedResults = results.flatMap { result =>
//          val variant = result.variant
//          val id = variant.id
//
//          val repository = new GitRepository(baseDir, result.repository)
//          if (!repository.exists) repository.init()
//          val variantMetadata = VariantMetadata.fromVariant(variant)
//
//          val includedVersionInfo = result.versionInfo.filter {
//            case (repository, id, _) => !excluded(repository, id)
//          }
//
//          val currentResults = VersionOrder.createResolutionResults(baseDir, includedVersionInfo) ++
//            Set(ResolutionResult(id, repository.name, repository.getHead, variantMetadata.hash))
//
//          val resolutionResultsMetadata = ResolutionResultsMetadata(currentResults.toSeq)
//          repository.add(resolutionResultsMetadata.write(id, variantMetadata.hash, repository))
//          repository.commit("Resolution results of " + variant.id)
//          currentResults
//        }
//        progress.update(1)
//        completedResults
//    }
//    progress.endTask()
//    progress.beginTask("GCing new Ivy repo(s)", grouped.size)
//    grouped.par.foreach { //NOTICE .par TODO: same as above (IO vs CPU)
//      case (name, _) =>
//        val repository = new GitRepository(baseDir, name)
//        repository.gc()
//        progress.update(1)
//    }
//    progress.endTask()
//    all
    ???
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
    Id(moduleId.getName)
  }
  val IdConfig = "config"

  def withConfiguration(id: Id, confName: String): Id = {
    Id(id.value + Id.Sep + IdConfig + Id.Sep + confName)
  }

  def ivyIdAsId(moduleId: ModuleId, confName: String): Id = {
    assert(!confName.contains(Id.Sep))
    withConfiguration(Id(moduleId.getName), confName)
  }

  def ivyIdAsRepositoryName(moduleId: ModuleId): RepositoryName = {
    RepositoryName(moduleId.getOrganisation)
  }

  def ivyIdAsVersion(mrid: ModuleRevisionId): Version = {
    Version(mrid.getRevision)
  }

}

case class IvyImportResult(variant: Variant, artifacts: Set[Artifact], localFiles: Map[ArtifactHash, File], repository: RepositoryName, versionInfo: Set[(RepositoryName, Id, Version)], excludeRules: Map[Id, Set[ExcludeRule]])

class IvyHelper(ivy: Ivy, changing: Boolean = true, skippableConf: Option[Set[String]] = Some(Set("javadoc", "sources"))) extends Logging {
  import AttributeDefaults.{ NameAttribute, OrgAttribute, VersionAttribute, ArtifactConfAttribute }
  import IvyHelper._

  val ConfigurationAttribute = "configuration-hash"

  /** As in sbt */
  private[adept] def cleanModule(mrid: ModuleRevisionId, resolveId: String, manager: ResolutionCacheManager) {
    val files =
      Option(manager.getResolvedIvyFileInCache(mrid)).toList :::
        Option(manager.getResolvedIvyPropertiesInCache(mrid)).toList :::
        Option(manager.getConfigurationResolveReportsInCache(resolveId)).toList.flatten
    import scala.reflect.io.Directory
    files.foreach { file =>
      (new Directory(file)).deleteRecursively() //I hop ethis works
    }
  }

  private def importAsSbt(module: ModuleDescriptor, progress: ProgressMonitor) = {
    //    val currentResolveOptions = resolveOptions()
    //    val resolveId = ResolveOptions.getDefaultResolveId(module)
    //    currentResolveOptions.setResolveId(resolveId)
    //    cleanModule(module.getModuleRevisionId, resolveId, ivy.getSettings.getResolutionCacheManager)

    progress.beginTask("Resolving Ivy module(s)", module.getDependencies().size)
    val resolveReport = ivy.resolve(module, resolveOptions())

    progress.update(module.getDependencies().size)
    progress.endTask()
    if (resolveReport.hasError) {
      val messages = resolveReport.getAllProblemMessages.toArray.map(_.toString).distinct
      val failed = resolveReport.getUnresolvedDependencies
      Left(new Exception(failed.mkString(",") + "failed to resolve. Messages:\n" + messages.mkString("\n")))
    } else Right(resolveReport)
  }

  def ivyImport(module: ModuleDescriptor, progress: ProgressMonitor): Set[IvyImportResult] = { //, Set[ResolutionResult]) = {
    ivy.synchronized { //ivy is not thread safe
      val mrid = module.getModuleRevisionId()
      importAsSbt(module, progress) match {
        case Right(resolveReport) =>
          val dependencyTree = createDependencyTree(mrid)(resolveReport)
          progress.start(module.getDependencies().size)
          val parentNode = resolveReport.getDependencies().asScala.map { case i: IvyNode => i }.head //Feels a bit scary?
          val mergableResults = module.getDependencies().flatMap { directDependency =>

            val drid = directDependency.getDependencyRevisionId()
            ivyImport(parentNode, drid.getOrganisation(), drid.getName(), drid.getRevision(), progress)
          }
          val resolutionResults = dependencyTree(mrid)
          println("--------------")
          println(resolveReport.getAllArtifactsReports().map { a =>
            a
            //case a: IvyArtifact => a.getModuleRevisionId().getOrganisation() + ":" + a.getModuleRevisionId().getName() + ":" + a.getModuleRevisionId().getRevision()
          })
          mergableResults.toSet
        case Left(exception) => throw exception
      }
    }
  }

  /**
   * Import from ivy based on coordinates
   */
  private def ivyImport(parentNode: IvyNode, org: String, name: String, version: String, progress: ProgressMonitor): Set[IvyImportResult] = {
    val mrid = ModuleRevisionId.newInstance(org, name, version)
    val mergableResults = { //ivy.synchronized { // ivy is not thread safe
      val resolveReport = ivy.resolve(mrid, resolveOptions(), changing)
      val dependencyTree = createDependencyTree(mrid)(resolveReport)
      val workingNode = dependencyTree(ModuleRevisionId.newInstance(org, name + "-caller", "working")).head
      progress.beginTask("Importing " + mrid, dependencyTree(workingNode.getId).size)
      val mergableResults = results(workingNode, parentNode, progress, progressIndicatorRoot = true)(dependencyTree)
      progress.endTask()
      mergableResults
    }
    mergableResults
  }

  private def results(currentIvyNode: IvyNode, parentNode: IvyNode, progress: ProgressMonitor, progressIndicatorRoot: Boolean)(dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Set[IvyImportResult] = {
    val mrid = currentIvyNode.getId
    val children = dependencies.getOrElse(mrid, Set.empty)

    val currentResults = createIvyResult(currentIvyNode, children, dependencies)
    val allResults = children.flatMap { childNode =>
      val childId = childNode.getId
      val dependencyTree = createDependencyTree(childId)(ivy.resolve(childId, resolveOptions(), changing))
      val finished = results(childNode, currentIvyNode, progress, progressIndicatorRoot = false)(dependencies ++ dependencyTree)
      if (progressIndicatorRoot) progress.update(1)
      finished
    } ++ currentResults
    allResults
  }

  private def createIvyResult(currentIvyNode: IvyNode, unloadedChildren: Set[IvyNode], dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Set[IvyImportResult] = {
    val mrid = currentIvyNode.getId
    val id = ivyIdAsId(mrid.getModuleId)
    val versionAttribute = Attribute(VersionAttribute, Set(mrid.getRevision()))
    val nameAttribute = Attribute(NameAttribute, Set(mrid.getName()))
    val orgAttribute = Attribute(OrgAttribute, Set(mrid.getOrganisation()))

    val configurationHash = Hasher.hash(mrid.toString.getBytes) //TODO: make more unique? 
    val attributes = Set(orgAttribute, nameAttribute, versionAttribute)

    val dependencyReport = ivy.resolve(mrid, resolveOptions(), changing)
    val moduleDescriptor = dependencyReport.getModuleDescriptor()

    val parentNode = dependencyReport.getDependencies().asScala.map { case i: IvyNode => i }.head //Feels a bit scary?

    val mergableResults = moduleDescriptor.getConfigurations().map { ivyConfiguration =>
      val confName = ivyConfiguration.getName

      val (loaded, notLoaded) = {
        val children = dependencyReport.getDependencies().asScala.flatMap { //we cannot use unloadedChildren directly, because they might not be loaded (if they are provided/evicted)
          case ivyNode: IvyNode =>
            unloadedChildren.find(_.getId == ivyNode.getId)
        }.toSet
        children.partition(_.isLoaded)
      }

      //print warnings:
      notLoaded.foreach { ivyNode =>
        if (!dependencies.isDefinedAt(ivyNode.getId)) {
          logger.debug(mrid + " has a node " + ivyNode + " which was not loaded, but it is not required in upper-call tree so we ignore")

          if (ivyNode == null) {
            logger.error("Got a null while loading: " + mrid)
          } else if (ivyNode.isEvicted(confName))
            logger.debug(mrid + " evicts " + ivyNode + " so it was not loaded.")
          else if (ivyNode.getDescriptor() != null && ivyNode.getDescriptor().canExclude()) {
            logger.debug(mrid + " required" + ivyNode + " which can be excluded.")
          } else {
            logger.debug(mrid + " required " + ivyNode + ", but is was not loaded (nor evicted) so cannot import. This is potentially a problem") //TODO: is this acceptable? if not find a way to load ivy nodes...
          }
        } else throw new Exception("Could not load " + ivyNode + "declared in: " + mrid)
      }
      //exclude rules:
      var excludeRules = Map.empty[Id, Set[ExcludeRule]]
      //requirements:
      val requirements = loaded.flatMap { ivyNode =>
        val currentExcludeRules = for { //handle nulls
          parentNode <- Option(currentIvyNode).toSet[IvyNode]
          currentIvyNode <- Option(ivyNode).toSet[IvyNode]
          dependencyDescriptor <- Option(currentIvyNode.getDependencyDescriptor(parentNode)).toSet[DependencyDescriptor]
          excludeRule <- dependencyDescriptor.getAllExcludeRules()
        } yield {
          excludeRule
        }
        if (!ivyNode.isEvicted(confName)) {
          val requirements = ivyNode.getConfigurations(confName).toSet.map(ivyNode.getConfiguration).map { requirementConf =>
            Requirement(ivyIdAsId(ivyNode.getId.getModuleId, requirementConf.getName()), Set.empty, Set.empty)
          } + Requirement(ivyIdAsId(ivyNode.getId.getModuleId), Set.empty, Set.empty)
          requirements.foreach{ requirement =>
            excludeRules += requirement.id -> currentExcludeRules
          }
          requirements
        } else Set.empty[Requirement]
      }

      //get exclusion rules:

      val artifactInfos = ivy.resolve(mrid, resolveOptions(ivyConfiguration.getName), changing).getArtifactsReports(mrid).flatMap { artifactReport =>
        val file = artifactReport.getLocalFile
        if (file != null) {
          val hash = {
            val is = new FileInputStream(file)
            try {
              ArtifactHash(Hasher.hash(is))
            } finally {
              is.close()
            }
          }
          Some((artifactReport.getArtifactOrigin().getLocation(), artifactReport.getArtifact().getConfigurations(), file, hash, file.getName))
        } else if (file == null && skippableConf.isDefined && skippableConf.get(ivyConfiguration.getName())) {
          None
        } else {
          throw new Exception("Could not download: " + mrid + " in " + confName)
        }
      }.toSet

      //TODO: skipping empty configurations? if (artifactInfos.nonEmpty || dependencies.nonEmpty)... 
      val artifacts = artifactInfos.map {
        case (location, _, file, hash, filename) =>
          Artifact(hash, file.length, Set(location))
      }

      val artifactRefs = artifactInfos.map {
        case (_, ivyConfs, file, hash, filename) =>
          ArtifactRef(hash, Set(ArtifactAttribute(ArtifactConfAttribute, ivyConfs.toSet)), Some(filename))
      }

      val localFiles = artifactInfos.map {
        case (_, _, file, hash, _) =>
          hash -> file
      }.toMap

      val configurationRequirements = parentNode.getConfiguration(confName).getExtends().map { targetConf =>
        Requirement(ivyIdAsId(mrid.getModuleId, targetConf), Set(Constraint(ConfigurationAttribute, Set(configurationHash))), Set.empty)
      }

      val variant = Variant(
        ivyIdAsId(mrid.getModuleId, confName),
        attributes = attributes,
        artifacts = artifactRefs,
        requirements = requirements)

      val targetVersionInfo: Set[(RepositoryName, Id, Version)] = loaded.flatMap { ivyNode =>
        if (!ivyNode.isEvicted(confName)) {
          val targetRepositoryName = ivyIdAsRepositoryName(ivyNode.getId.getModuleId)
          val targetVersion = ivyIdAsVersion(ivyNode.getId)
          ivyNode.getConfigurations(confName).toSet.map(ivyNode.getConfiguration).map { requirementConf =>
            val targetId = ivyIdAsId(ivyNode.getId.getModuleId, requirementConf.getName)
            (targetRepositoryName, targetId, targetVersion)
          } + ((targetRepositoryName, ivyIdAsId(ivyNode.getId.getModuleId), targetVersion))
        } else {
          Set.empty[(RepositoryName, Id, Version)]
        }
      }

      //TODO: overrides must also be imported and then we must this to versionInfo (or perhaps not)
      //      ++ parentNode.getDescriptor().getAllDependencyDescriptorMediators().getAllRules().asScala.map {
      //        case (matcher: MapMatcher, overrideMediator: OverrideDependencyDescriptorMediator) =>
      //          val matcherName = matcher.getPatternMatcher().getName()
      //          matcherName match {
      //            case PatternMatcher.EXACT => true
      //            case _ => throw new Exception("found matcher: " + matcherName + ". currently only: " + PatternMatcher.EXACT + " is supported. parent:" + parentNode)
      //          }
      //          val org = matcher.getAttributes.get(IvyPatternHelper.ORGANISATION_KEY) match { case s: String => s }
      //          val name = matcher.getAttributes.get(IvyPatternHelper.MODULE_KEY) match { case s: String => s }
      //          val overriddenVersion = overrideMediator.getVersion()
      //          (RepositoryName(org), Id(name), Version(overriddenVersion))
      //      }

      IvyImportResult(
        variant = variant,
        artifacts = artifacts,
        localFiles = localFiles,
        repository = ivyIdAsRepositoryName(mrid.getModuleId),
        versionInfo = targetVersionInfo,
        excludeRules = excludeRules)
    }.toSet

    mergableResults +
      IvyImportResult( //<-- adding main configuration to make sure that there is not 2 variants with different "configurations" 
        variant = Variant(id, attributes = attributes + Attribute(ConfigurationAttribute, Set(configurationHash))),
        artifacts = Set.empty,
        localFiles = Map.empty,
        repository = ivyIdAsRepositoryName(mrid.getModuleId),
        versionInfo = Set.empty,
        excludeRules = Map.empty)
  }

  private def createDependencyTree(mrid: ModuleRevisionId)(report: ResolveReport) = { //TODO: rename to requirement? or perhaps not?
    var dependencies = Map.empty[ModuleRevisionId, Set[IvyNode]]
    def addDependency(mrid: ModuleRevisionId, ivyNode: IvyNode) = {
      val current = dependencies.getOrElse(mrid, Set.empty) + ivyNode
      dependencies += mrid -> current
    }

    report.getDependencies().asScala.foreach {
      case ivyNode: IvyNode =>
        if (mrid != ivyNode.getId) addDependency(mrid, ivyNode)
    }

    val currentCallers = report.getDependencies().asScala.foreach {
      case ivyNode: IvyNode => ivyNode.getAllCallers.map { caller =>
        if (caller.getModuleRevisionId != ivyNode.getId) addDependency(caller.getModuleRevisionId, ivyNode)
      }
    }
    dependencies
  }
}