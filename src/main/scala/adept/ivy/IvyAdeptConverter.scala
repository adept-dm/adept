package adept.ivy

import org.apache.ivy.Ivy
import adept.logging.Logging
import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import org.eclipse.jgit.lib.ProgressMonitor
import adept.resolution.resolver.models.ResolvedResult
import adept.resolution.models.Id
import org.apache.ivy.core.module.id.ModuleRevisionId
import adept.artifact.models.ArtifactHash
import java.io.FileInputStream
import adept.utils.Hasher
import org.apache.ivy.core.resolve.ResolveOptions
import org.apache.ivy.core.report.ResolveReport
import org.apache.ivy.core.resolve.IvyNode
import org.apache.ivy.core.module.descriptor.ExcludeRule
import adept.resolution.models.Requirement
import org.apache.ivy.core.module.descriptor.Configuration
import org.apache.ivy.core.module.descriptor.Configuration.Visibility
import adept.repository.models.RepositoryName
import adept.ext.Version
import adept.resolution.models.Attribute
import adept.artifact.models.Artifact
import adept.artifact.models.ArtifactRef
import adept.resolution.models.Constraint
import adept.artifact.models.ArtifactAttribute
import adept.resolution.models.Variant

class IvyAdeptConverter(ivy: Ivy, changing: Boolean = true, skippableConf: Option[Set[String]] = Some(Set("javadoc", "sources"))) extends Logging {
  import adept.ext.AttributeDefaults.VersionAttribute
  import IvyUtils._
  import IvyConstants._
  import collection.JavaConverters._

  /**
   * Loads and converts results from an Ivy module to IyyImportResults which Adept can use.
   * 
   * Conversion from Ivy to Adept consists of 2 steps:
   * 1) Load Ivy import results from Ivy: @see [[adept.ivy.IvyAdeptConverter.loadAsIvyImportResults]] in this class
   * 2) Insert Ivy import results into corresponding Adept repositories: @see [[adept.ivy.IvyResolutionResults.insertAsResolutionResults]]
   * 
   * To convert dependencies to requirements @see [[adept.ivy.IvyRequirements.convertIvyAsRequirements]]
   * 
   * To verify that the requirements and the conversion was correct @see [[adept.ivy.IvyAdeptConverter.verifyConversion]]
   **/
  def loadAsIvyImportResults(module: ModuleDescriptor, progress: ProgressMonitor): Either[Set[IvyImportError], Set[IvyImportResult]] = {
    ivy.synchronized { //ivy is not thread safe
      val mrid = module.getModuleRevisionId()
      progress.beginTask("Resolving Ivy module(s)", module.getDependencies().size)
      getResolveReport(module, resolveOptions()) match {
        case Right(resolveReport) =>
          progress.update(module.getDependencies().size)
          progress.endTask()
          val dependencyTree = createDependencyTree(mrid)(resolveReport)
          progress.start(module.getDependencies().size)
          var allResults = Set.empty[IvyImportResult]
          var errors = Set.empty[IvyImportError]

          module.getDependencies().foreach { directDependency =>
            val drid = directDependency.getDependencyRevisionId()
            val newResults = ivySingleImport(drid.getOrganisation(), drid.getName(), drid.getRevision(), progress)
            allResults ++= newResults.right.getOrElse(Set.empty[IvyImportResult])
            errors ++= newResults.left.getOrElse(Set.empty[IvyImportError])
          }
          if (errors.nonEmpty) Left(errors)
          else Right(allResults)
        case Left(error) => throw new Exception(error)
      }
    }
  }

  /** Checks whether resolving the module yields the same result as an Adept resolved result */
  def verifyConversion(confName: String, module: ModuleDescriptor, resolvedResult: ResolvedResult): Either[IvyVerificationErrorReport, Set[Id]] = {
    val resolvedVariants = resolvedResult.state.resolvedVariants
    val adeptIds = resolvedVariants.keySet
    val allDepArtifacts = resolvedVariants.flatMap {
      case (_, variant) =>
        variant.artifacts.map { artifact =>
          artifact.hash -> variant
        }
    }
    var adeptExtraArtifacts = allDepArtifacts
    var ivyExtraArtifacts = Map.empty[ArtifactHash, ModuleRevisionId]
    var nonMatchingArtifacts = Set.empty[IvyVerificationError]

    getResolveReport(module, resolveOptions(confName)) match {
      case Right(resolveReport) =>
        val configurationReport = resolveReport.getConfigurationReport(confName)
        configurationReport.getAllArtifactsReports().foreach { artifactReport =>
          val ivyArtifact = artifactReport.getArtifact()

          val ivyArtifactHash = {
            val fis = new FileInputStream(artifactReport.getLocalFile())
            try {
              ArtifactHash(Hasher.hash(fis))
            } finally {
              fis.close()
            }
          }
          val mrid = ivyArtifact.getModuleRevisionId()
          adeptExtraArtifacts -= ivyArtifactHash //we found an artifact in ivy which we was found in adept
          if (!allDepArtifacts.isDefinedAt(ivyArtifactHash)) {
            ivyExtraArtifacts += ((ivyArtifactHash, mrid)) //we found an artifact in ivy which do not have in adept
          }
          (ivyArtifact.getConfigurations().toSet + configurationReport.getConfiguration).foreach { confName =>
            val targetId = ivyIdAsId(mrid.getModuleId, confName)
            resolvedVariants.get(targetId) match {
              case Some(variant) =>
                val matchingArtifacts = variant.artifacts.filter { artifact =>
                  artifact.attribute(ArtifactConfAttribute).values == ivyArtifact.getConfigurations().toSet
                }
                //we found  1 artifact matching but the hashes are different
                if (matchingArtifacts.size == 1 && ivyArtifactHash != matchingArtifacts.head.hash) {
                  nonMatchingArtifacts += IvyVerificationError(ivyArtifactHash, variant, matchingArtifacts.map(_.hash))
                }
              case None => //pass
            }
          }
        }
        if (nonMatchingArtifacts.isEmpty && ivyExtraArtifacts.isEmpty && adeptExtraArtifacts.isEmpty) {
          Right(adeptIds)
        } else {
          Left(IvyVerificationErrorReport(
            msg = "Ivy was resolved, but there was mis-matching artifacts found",
            adeptExtraArtifacts,
            ivyExtraArtifacts,
            nonMatchingArtifacts))
        }
      case Left(error) =>
        Left(IvyVerificationErrorReport(
          msg = error,
          adeptExtraArtifacts,
          ivyExtraArtifacts,
          nonMatchingArtifacts))
    }
  }

  private def getResolveReport(module: ModuleDescriptor, initialResolveOptions: ResolveOptions) = {
    val currentResolveOptions = initialResolveOptions
    val resolveId = ResolveOptions.getDefaultResolveId(module)
    currentResolveOptions.setResolveId(resolveId)
    cleanModule(module.getModuleRevisionId, resolveId, ivy.getSettings.getResolutionCacheManager)

    def reportErrorString(resolveReport: ResolveReport) = {
      val messages = resolveReport.getAllProblemMessages.toArray.map(_.toString).distinct
      val failed = resolveReport.getUnresolvedDependencies
      failed.mkString(",") + " failed to resolve. Messages:\n" + messages.mkString("\n")
    }

    val resolveReport = ivy.resolve(module, initialResolveOptions)

    if (resolveReport.hasError) {
      Left("Got errors when trying to resolve from Ivy: " + reportErrorString(resolveReport))
    } else {
      Right(resolveReport)
    }
  }

  private def ivySingleImport(org: String, name: String, version: String, progress: ProgressMonitor): Either[Set[IvyImportError], Set[IvyImportResult]] = {
    val mrid = ModuleRevisionId.newInstance(org, name, version)
    val resolveReport = ivy.resolve(mrid, resolveOptions(), changing)
    val dependencyTree = createDependencyTree(mrid)(resolveReport)
    val workingNode = dependencyTree(ModuleRevisionId.newInstance(org, name + "-caller", "working")).head
    progress.beginTask("Importing " + mrid, dependencyTree(workingNode.getId).size)
    val allResults = results(workingNode, progress, progressIndicatorRoot = true)(dependencyTree)
    progress.endTask()
    allResults
  }

  private def results(currentIvyNode: IvyNode, progress: ProgressMonitor, progressIndicatorRoot: Boolean)(dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Either[Set[IvyImportError], Set[IvyImportResult]] = {
    val mrid = currentIvyNode.getId
    val children = dependencies.getOrElse(mrid, Set.empty)

    val currentResults = createIvyResult(currentIvyNode, children, dependencies)
    var allResults = currentResults.right.getOrElse(Set.empty[IvyImportResult])
    var errors = currentResults.left.getOrElse(Set.empty[IvyImportError])

    children.foreach { childNode =>
      val childId = childNode.getId
      val dependencyTree = createDependencyTree(childId)(ivy.resolve(childId, resolveOptions(), changing))
      val newResults = results(childNode, progress, progressIndicatorRoot = false)(dependencies ++ dependencyTree)
      if (progressIndicatorRoot) progress.update(1)
      allResults ++= newResults.right.getOrElse(Set.empty[IvyImportResult])
      errors ++= newResults.left.getOrElse(Set.empty[IvyImportError])
    }

    if (errors.isEmpty) Right(allResults)
    else Left(errors)
  }

  private def printWarnings(mrid: ModuleRevisionId, confName: String, notLoaded: Set[IvyNode], dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Unit = {
    notLoaded.foreach { ivyNode => //TODO: I am not a 100% certain that we do not really need them? Where do these deps come from, somebody wanted them originally?
      if (!dependencies.isDefinedAt(ivyNode.getId)) {
        logger.debug(mrid + " has a node " + ivyNode + " which was not loaded, but it is not required in upper-call tree so we ignore")

        if (ivyNode == null) {
          logger.error("Got a null while loading: " + mrid)
        } else if (ivyNode.isEvicted(confName))
          logger.debug(mrid + " evicts " + ivyNode + " so it was not loaded.")
        else if (ivyNode.getDescriptor() != null && ivyNode.getDescriptor().canExclude()) {
          logger.debug(mrid + " required" + ivyNode + " which can be excluded.")
        } else {
          logger.error(mrid + " required " + ivyNode + ", but is was not loaded (nor evicted) so cannot import. This is potentially a problem") //TODO: is this acceptable? if not find a way to load ivy nodes...
        }
      } else throw new Exception("Could not load " + ivyNode + "declared in: " + mrid)
    }
  }

  private def extractRequirementsAndExcludes(thisVariantId: Id, confName: String, currentIvyNode: IvyNode, loaded: Set[IvyNode]) = {
    var excludeRules = Map.empty[(Id, Id), Set[ExcludeRule]]

    val requirements = loaded.flatMap { ivyNode =>
      val currentExcludeRules = getExcludeRules(currentIvyNode, ivyNode)
      if (!ivyNode.isEvicted(confName)) {
        val requirements = ivyNode.getConfigurations(confName).toSet.map(ivyNode.getConfiguration).map { requirementConf =>
          Requirement(ivyIdAsId(ivyNode.getId.getModuleId, requirementConf.getName()), Set.empty, Set.empty)
        } + Requirement(ivyIdAsId(ivyNode.getId.getModuleId), Set.empty, Set.empty)
        requirements.foreach { requirement =>
          if (currentExcludeRules.nonEmpty) {
            excludeRules += (thisVariantId, requirement.id) -> currentExcludeRules //<-- MUTATE!
          }
        }
        requirements
      } else Set.empty[Requirement]
    }
    requirements -> excludeRules
  }

  private def extractArtifactInfosAndErrors(mrid: ModuleRevisionId, ivyConfiguration: Configuration, confName: String) = { //TODO: what is the difference on confName and ivyConfiguration.getName?
    var errors = Set.empty[ArtifactLocationError]
    ivy.resolve(mrid, resolveOptions(ivyConfiguration.getName), changing).getArtifactsReports(mrid).flatMap { artifactReport =>
      if (artifactReport.getArtifact().getConfigurations().toList.contains(confName)) {
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
          val location = artifactReport.getArtifactOrigin().getLocation()
          if (!location.startsWith("http")) errors += ArtifactLocationError(location, file) //we must have somewhere we can download this files from
          Some((location, artifactReport.getArtifact().getConfigurations(), file, hash, file.getName))
        } else if (file == null && skippableConf.isDefined && skippableConf.get(ivyConfiguration.getName())) {
          None
        } else {
          throw new Exception("Could not download: " + mrid + " in " + confName)
        }
      } else None
    }.toSet -> errors
  }

  private def extractTargetVersionInfo(confName: String, loaded: Set[IvyNode]) = {
    loaded.flatMap { ivyNode =>
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
  }

  private def createIvyResult(currentIvyNode: IvyNode, unloadedChildren: Set[IvyNode], dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Either[Set[IvyImportError], Set[IvyImportResult]] = {
    var errors = Set.empty[IvyImportError]

    val mrid = currentIvyNode.getId
    val id = ivyIdAsId(mrid.getModuleId)
    val versionAttribute = Attribute(VersionAttribute, Set(mrid.getRevision()))
    val nameAttribute = Attribute(IvyNameAttribute, Set(mrid.getName()))
    val orgAttribute = Attribute(IvyOrgAttribute, Set(mrid.getOrganisation()))

    val configurationHash = Hasher.hash(mrid.toString.getBytes) //TODO: make more unique? 
    val attributes = Set(orgAttribute, nameAttribute, versionAttribute)

    val dependencyReport = ivy.resolve(mrid, resolveOptions(), changing)
    val moduleDescriptor = dependencyReport.getModuleDescriptor()
    val parentNode = getParentNode(dependencyReport)
    val mergableResults = dependencyReport.getConfigurations()
      .map(c => parentNode.getConfiguration(c)) //careful here. you could think: moduleDescriptor.getConfigurations is the same but it is not (you get bogus configurations back) 
      .filter(_.getVisibility() == Visibility.PUBLIC) //we cannot get dependencies for private configurations so we just skip them all together
      .map { ivyConfiguration =>
        val confName = ivyConfiguration.getName
        val thisVariantId = ivyIdAsId(mrid.getModuleId, confName)

        val (loaded, notLoaded) = {
          val rootConf = confName //TODO: I am honestly not a 100% sure how rootConf is different from actual conf?
          val children = parentNode.getDependencies(rootConf, confName, "*").asScala.flatMap { //we cannot use unloadedChildren directly, because they might not be loaded (if they are provided/evicted)
            case ivyNode: IvyNode =>
              unloadedChildren.find { child =>
                child.getId == ivyNode.getId
              }
          }.toSet
          children.partition(_.isLoaded)
        }

        printWarnings(mrid, confName, notLoaded, dependencies)
        val (requirements, excludeRules) = extractRequirementsAndExcludes(thisVariantId, confName, currentIvyNode, loaded)

        val (artifactInfos, newErrors) = extractArtifactInfosAndErrors(mrid, ivyConfiguration, confName)
        errors ++= newErrors //MUTATE!

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

        val configurationRequirements = ivyConfiguration.getExtends().map { targetConf =>
          Requirement(ivyIdAsId(mrid.getModuleId, targetConf), Set(Constraint(ConfigurationHashAttribute, Set(configurationHash))), Set.empty)
        }

        val variant = Variant(
          id = thisVariantId,
          attributes = attributes + Attribute(ConfigurationHashAttribute, Set(configurationHash)) + Attribute(ConfigurationAttribute, Set(confName)),
          artifacts = artifactRefs,
          requirements = requirements ++ configurationRequirements)

        val targetVersionInfo = extractTargetVersionInfo(confName, loaded)

        IvyImportResult(
          variant = variant,
          artifacts = artifacts,
          localFiles = localFiles,
          repository = ivyIdAsRepositoryName(mrid.getModuleId),
          versionInfo = targetVersionInfo,
          excludeRules = excludeRules)
      }.toSet

    if (errors.nonEmpty) Left(errors)
    else Right(
      mergableResults +
        IvyImportResult( //<-- adding main configuration to make sure that there is not 2 variants with different "configurations" 
          variant = Variant(id, attributes = attributes + Attribute(ConfigurationHashAttribute, Set(configurationHash))),
          artifacts = Set.empty,
          localFiles = Map.empty,
          repository = ivyIdAsRepositoryName(mrid.getModuleId),
          versionInfo = Set.empty,
          excludeRules = Map.empty))
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