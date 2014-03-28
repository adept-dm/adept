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
import org.apache.ivy.core.module.descriptor.{ Artifact => IvyArtifact }
import org.apache.ivy.core.module.descriptor.Configuration.Visibility
import adept.repository.models.RepositoryName
import adept.ext.Version
import adept.resolution.models.Attribute
import adept.artifact.models.Artifact
import adept.artifact.models.ArtifactRef
import adept.resolution.models.Constraint
import adept.artifact.models.ArtifactAttribute
import adept.resolution.models.Variant
import adept.repository.models.ResolutionResult
import java.io.StringBufferInputStream
import java.io.StringReader
import java.io.InputStreamReader
import java.io.BufferedReader
import org.apache.ivy.core.report.ArtifactDownloadReport
import java.io.File
import scala.xml.XML
import adept.artifact.ArtifactCache

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
   * 2) Insert Ivy import results into corresponding Adept repositories: @see [[adept.ivy.IvyImportResultInserter.insertAsResolutionResults]]
   * 3) Use version info (the second part of the tuple) from loadAsIvyImportResults to generate resolution results that are the same as Ivy
   *
   * To convert dependencies to requirements @see [[adept.ivy.IvyRequirements.convertIvyAsRequirements]]
   *
   * To verify that the requirements and the conversion was correct @see [[adept.ivy.IvyAdeptConverter.verifyConversion]]
   */
  def loadAsIvyImportResults(module: ModuleDescriptor, progress: ProgressMonitor): Either[Set[IvyImportError], (Set[IvyImportResult], Map[String, Set[(RepositoryName, Id, Version)]])] = {
    ivy.synchronized { //ivy is not thread safe
      val mrid = module.getModuleRevisionId()
      progress.beginTask("Resolving Ivy module(s)", module.getDependencies().size)
      getResolveReport(module, resolveOptions()) match {
        case Right(resolveReport) =>
          progress.update(module.getDependencies().size)
          progress.endTask()
          val configDependencyTree = createConfigDependencyTree(module)(resolveReport)
          progress.start(module.getDependencies().size)
          var allResults = Set.empty[IvyImportResult]
          var errors = Set.empty[IvyImportError]

          val dependencyTree = createDependencyTree(mrid)(resolveReport)
          dependencyTree(mrid).foreach { ivyNode =>
            val currentIvyId = ivyNode.getId()
            val newResults = ivySingleImport(currentIvyId.getOrganisation(), currentIvyId.getName(), currentIvyId.getRevision(), progress)
            allResults ++= newResults.right.getOrElse(Set.empty[IvyImportResult])
            errors ++= newResults.left.getOrElse(Set.empty[IvyImportError])
          }

          //TODO: this part of the code doesn't feel right, but we need it to match the exact versions that ivy produces
          val allIds = allResults.map(_.variant.id)
          val versionInfo = configDependencyTree.keys.map { confName =>
            val depdendencyTree = configDependencyTree(confName)
            confName -> depdendencyTree(mrid).flatMap { ivyNode =>
              val currentIvyId = ivyNode.getId()
              val currentAdeptId = ivyIdAsId(currentIvyId.getModuleId)
              val foundIds = allIds.collect {
                case id if id.value.startsWith(currentAdeptId.value + Id.Sep + IdConfig) => id
              } + currentAdeptId
              foundIds.map { id =>
                (ivyIdAsRepositoryName(currentIvyId.getModuleId), id, Version(currentIvyId.getRevision()))
              }
            }
          }.toMap

          if (errors.nonEmpty) Left(errors)
          else Right(allResults -> versionInfo)
        case Left(error) => throw new Exception(error)
      }
    }
  }

  //TODO: merge or replace createDependencyTree with this? feels non DRY that is for sure
  private def createConfigDependencyTree(module: ModuleDescriptor)(resolveReport: ResolveReport) = { //TODO: rename to requirement? or perhaps not?
    val mrid = module.getModuleRevisionId
    val confNames = module.getConfigurationsNames()
    confNames.map { confName =>
      val report = resolveReport.getConfigurationReport(confName)
      if (report.getUnresolvedDependencies().nonEmpty) throw new Exception(module.getModuleRevisionId() + " has unresolved dependencies:\n" + report.getUnresolvedDependencies().toList.mkString("\n"))
      var dependencies = Map.empty[ModuleRevisionId, Set[IvyNode]]
      def addDependency(mrid: ModuleRevisionId, ivyNode: IvyNode) = {
        val current = dependencies.getOrElse(mrid, Set.empty) + ivyNode
        dependencies += mrid -> current
      }

      report.getModuleRevisionIds.asScala.foreach {
        case currentMrid: ModuleRevisionId =>
          if (mrid != currentMrid) addDependency(mrid, report.getDependency(currentMrid))
      }

      val currentCallers = report.getModuleRevisionIds().asScala.foreach {
        case currentMrid: ModuleRevisionId =>
          val ivyNode = report.getDependency(currentMrid)
          ivyNode.getAllCallers.map { caller =>
            if (caller.getModuleRevisionId != ivyNode.getId) addDependency(caller.getModuleRevisionId, ivyNode)
          }
          dependencies
      }
      confName -> {
        if (dependencies.isEmpty) Map(mrid -> Set.empty)
        else dependencies
      }
    }.toMap
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
        configurationReport.getAllArtifactsReports.foreach { artifactReport =>
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

  private def getCaller(org: String, name: String) = ModuleRevisionId.newInstance(org, name + "-caller", "working")

  private def ivySingleImport(org: String, name: String, version: String, progress: ProgressMonitor): Either[Set[IvyImportError], Set[IvyImportResult]] = {
    val mrid = ModuleRevisionId.newInstance(org, name, version)
    progress.beginTask("Ivy resolving " + mrid, ProgressMonitor.UNKNOWN)
    val resolveReport = ivy.resolve(mrid, resolveOptions(), changing)
    progress.endTask()
    val dependencyTree = createDependencyTree(mrid)(resolveReport)
    val workingNode = dependencyTree(getCaller(org, name)).head
    progress.beginTask("Importing " + mrid, dependencyTree.get(workingNode.getId).map(_.size).getOrElse(0) + 1)
    val allResults = results(workingNode, resolveReport, Set.empty, progress, progressIndicatorRoot = true)(dependencyTree)
    progress.update(1)
    progress.endTask()
    allResults
  }

  private def results(currentIvyNode: IvyNode, resolveReport: ResolveReport, visited: Set[IvyNode], progress: ProgressMonitor, progressIndicatorRoot: Boolean)(dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Either[Set[IvyImportError], Set[IvyImportResult]] = {
    val mrid = currentIvyNode.getId
    val children = dependencies.getOrElse(mrid, Set.empty)

    val currentResults = createIvyResult(currentIvyNode, children, dependencies, resolveReport)
    var allResults = currentResults.right.getOrElse(Set.empty[IvyImportResult])
    var errors = currentResults.left.getOrElse(Set.empty[IvyImportError])

    val (loaded, notLoaded) = children.partition(_.isLoaded)
    if (progressIndicatorRoot) progress.update(notLoaded.size)

    printWarnings(mrid, None, notLoaded, dependencies)
    loaded.filter(!visited(_)).foreach { childNode =>
      val childId = childNode.getId
      val dependencyTree = createDependencyTree(childId)(ivy.resolve(childId, resolveOptions(), changing))
      val newResults = results(childNode, resolveReport, visited ++ loaded + currentIvyNode, progress, progressIndicatorRoot = false)(dependencies ++ dependencyTree)
      if (progressIndicatorRoot) progress.update(1)
      allResults ++= newResults.right.getOrElse(Set.empty[IvyImportResult])
      errors ++= newResults.left.getOrElse(Set.empty[IvyImportError])
    }

    if (errors.isEmpty) Right(allResults)
    else Left(errors)
  }

  private def printWarnings(mrid: ModuleRevisionId, confName: Option[String], notLoaded: Set[IvyNode], dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Unit = {
    notLoaded.foreach { ivyNode => //TODO: I am not a 100% certain that we do not really need them? Where do these deps come from, somebody wanted them originally?
      if (!dependencies.isDefinedAt(ivyNode.getId)) {
        logger.debug(mrid + " has a node " + ivyNode + " which was not loaded, but it is not required in upper-call tree so we ignore")

        if (ivyNode == null) {
          logger.debug("Got a null while loading: " + mrid)
        } else if (confName.isDefined && ivyNode.isEvicted(confName.get)) { //<- careful .get
          logger.debug(mrid + " evicts " + ivyNode + " so it was not loaded.")
        } else if (ivyNode.getDescriptor() != null && ivyNode.getDescriptor().canExclude()) {
          logger.debug(mrid + " required" + ivyNode + " which can be excluded.")
        } else if (ivyNode.isCompletelyEvicted() || ivyNode.isCompletelyBlacklisted()) {
          logger.debug(mrid + " required " + ivyNode + " and it was evicted or blacklisted, so it is not loaded and will not be imported.")
        } else {
          //TODO: make this logger.error, but do not log if we are coming from something which is provided. It is currently debug to avoid the noise
          logger.debug(mrid + " required " + ivyNode + ", but is was not loaded (nor evicted) so cannot import. This is potentially a problem") //TODO: is this acceptable? if not find a way to load ivy nodes...
        }
      } else throw new Exception("Could not load " + ivyNode + "declared in: " + mrid)
    }
  }

  private def getConfigurations(ivyNode: IvyNode, confName: String) = {
    ivyNode.getConfigurations(confName).toSet[String]
      .flatMap { conf =>
        if (ivyNode.getConfiguration(conf) == null)
          logger.warn("Got null for configuration: " + conf + " @ " + ivyNode)
        Option(ivyNode.getConfiguration(conf))
      }
  }

  private def extractRequirementsAndExcludes(thisVariantId: Id, confName: String, currentIvyNode: IvyNode, loaded: Set[IvyNode]) = {
    var excludeRules = Map.empty[(Id, Id), Set[ExcludeRule]]

    val requirements = loaded.flatMap { ivyNode =>
      val currentExcludeRules = getExcludeRules(currentIvyNode, ivyNode)
      if (!ivyNode.isEvicted(confName)) {
        val requirements = getConfigurations(ivyNode, confName).map { requirementConf =>
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
    val resolveReport = ivy.resolve(mrid, resolveOptions(ivyConfiguration.getName), changing)
    resolveReport.getAllArtifactsReports.flatMap { artifactReport =>
      def extract(file: File) = {
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
      }

      if (artifactReport.getArtifact().getModuleRevisionId() == mrid && artifactReport.getArtifact().getConfigurations().toList.contains(confName)) {
        val file = artifactReport.getLocalFile
        if (file != null) {
          extract(file)
        } else if (file == null && skippableConf.isDefined && skippableConf.get(ivyConfiguration.getName())) {
          None
        } else {
          throw new Exception("Could not download: " + mrid + " in " + confName)
        }
      } else {
        if (artifactReport.getArtifact().getConfigurations().toList.isEmpty) {
          logger.debug("Ivy has an issue where sometimes configurations are not read. Reading them manually!")
          //WORKAROUND :(  there is an issue in ivy where it sometimes leaves out the confs for artifacts (I think this happens for modules that do not have depdendencies)
          val foundArtifact = for {
            file <- Option(artifactReport.getLocalFile()).toSeq
            cacheIvyDescriptorDir = new File(file.getAbsolutePath().replace("jars" + File.separator + file.getName, ""))
            if cacheIvyDescriptorDir.isDirectory()
            ivyXmlFile = new File(cacheIvyDescriptorDir, "ivy-" + artifactReport.getArtifact().getModuleRevisionId().getRevision() + ".xml")
            if ivyXmlFile.isFile()
            artifact <- XML.loadFile(ivyXmlFile) \\ "ivy-module" \ "publications" \ "artifact"
            if (artifact \ "@name").text == artifactReport.getName()
            confs = (artifact \ "@conf").text.split(",")
            currentConf <- confs
            if currentConf == confName
          } yield {
            extract(file)
          }
          assert(foundArtifact.size < 2)
          foundArtifact.flatten
        } else None
      }
    }.toSet -> errors
  }

  private def extractTargetVersionInfo(confName: String, loaded: Set[IvyNode]) = {
    loaded.flatMap { ivyNode =>
      if (!ivyNode.isEvicted(confName)) {
        val targetRepositoryName = ivyIdAsRepositoryName(ivyNode.getId.getModuleId)
        val targetVersion = ivyIdAsVersion(ivyNode.getId)
        getConfigurations(ivyNode, confName).map { requirementConf =>
          val targetId = ivyIdAsId(ivyNode.getId.getModuleId, requirementConf.getName)
          (targetRepositoryName, targetId, targetVersion)
        } + ((targetRepositoryName, ivyIdAsId(ivyNode.getId.getModuleId), targetVersion))
      } else {
        Set.empty[(RepositoryName, Id, Version)]
      }
    }
  }

  private def createIvyResult(currentIvyNode: IvyNode, unloadedChildren: Set[IvyNode], dependencies: Map[ModuleRevisionId, Set[IvyNode]], resolveReport: ResolveReport): Either[Set[IvyImportError], Set[IvyImportResult]] = {
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
    if (!parentNode.isLoaded) throw new Exception("Cannot load: " + parentNode + " - it might not have been resolved. Errors:\n" + dependencyReport.getAllProblemMessages().asScala.distinct.mkString("\n"))
    val mergableResults = dependencyReport.getConfigurations()
      .map(c => parentNode.getConfiguration(c)) //careful here. you could think: moduleDescriptor.getConfigurations is the same but it is not (you get bogus configurations back) 
      .filter(_.getVisibility() == Visibility.PUBLIC) //we cannot get dependencies for private configurations so we just skip them all together
      .map { ivyConfiguration =>
        val confName = ivyConfiguration.getName
        val thisVariantId = ivyIdAsId(mrid.getModuleId, confName)

        val (loaded, notLoaded) = {
          val children = Option(resolveReport.getConfigurationReport(confName)).map {
            _.getModuleRevisionIds().asScala.flatMap { //we cannot use unloadedChildren directly, because they might not be loaded (if they are provided/evicted)
              case childMrid: ModuleRevisionId =>
                unloadedChildren.find { child =>
                  child.getId == childMrid
                }
            }.toSet
          }.getOrElse{
            logger.warn("Could not get configuration report for: " + confName + " " + mrid)
            Set.empty[IvyNode]
          }

          children.partition(_.isLoaded)
        }

        printWarnings(mrid, Some(confName), notLoaded, dependencies)
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