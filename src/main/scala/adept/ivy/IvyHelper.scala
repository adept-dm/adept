package adept.ivy

import adept.models._
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
import adept.repository._
import adept.ext.Version
import adept.logging.Logging
import adept.repository.models._
import adept.repository.models.configuration._
import adept.ext.conversions.SemanticVersion
import adept.ext.conversions.ScalaBinaryVersion
import adept.artifacts.ArtifactCache

case class AdeptIvyResolveException(msg: String) extends Exception(msg)
case class AdeptIvyException(msg: String) extends Exception(msg)

object IvyHelper extends Logging {
  import AttributeDefaults.{ NameAttribute, OrgAttribute, VersionAttribute }

  def createId(name: String) = {
    Id(name)
  }

  lazy val errorIvyLogger = new DefaultMessageLogger(Message.MSG_ERR)
  lazy val warnIvyLogger = new DefaultMessageLogger(Message.MSG_WARN)
  lazy val infoIvyLogger = new DefaultMessageLogger(Message.MSG_INFO)
  lazy val debugIvyLogger = new DefaultMessageLogger(Message.MSG_DEBUG)

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

  def name2IdConversions(name: String) = { //TODO: make configurable!
    ScalaBinaryVersion.convertId(name)
  }

  def org2RepoConversions(org: String) = { //TODO: make configurable! / remove?
    org
  }

  def updateRepository(baseDir: File, result: IvyImportResult, commits: Map[ConfigurationId, Set[(Id, AdeptCommit)]]) = {
    def hasSameAttribute(attributeName: String, attributes1: Set[Attribute], attributes2: Set[Attribute]): Boolean = {
      attributes1.find(_.name == attributeName) == attributes2.find(_.name == attributeName)
    }

    def assertCorrectConfigurations() = {
      val variantConfiguration = result.variantMetadata.configurations.map(_.id).toSet
      commits.foreach {
        case (configuration, _) =>
          if (!variantConfiguration(configuration)) throw new Exception("Found configuration in commit information that does not match variants (" + configuration + "). Commits:\n" + commits.mkString("\n") + "Variant: " + variantConfiguration)
      }
    }

    val repoId = result.mrid.getOrganisation()
    val adeptGitRepo = new AdeptGitRepository(baseDir, repoId)
    val variantMetadata = result.variantMetadata

    result.localFiles.foreach { case (artifact, file, filename) => ArtifactCache.cache(baseDir, file, artifact.hash, filename) }
    adeptGitRepo.updateMetadata(branch = AdeptGitRepository.AdeptUriBranchName,
      removals = { content =>
        Seq.empty
      }, additions = { content =>
        val artifactFiles = result.artifacts.toSeq.map(ArtifactMetadata.fromArtifact(_).write(adeptGitRepo))
        artifactFiles
      }, commitMsg = "Ivy import of: " + result.mrid)

    adeptGitRepo.updateMetadata(removals = { content =>
      val removeVariants = content.variantMetadata
        .filter { old =>
          old.id == variantMetadata.id &&
            hasSameAttribute(AttributeDefaults.NameAttribute, old.attributes, variantMetadata.attributes) &&
            hasSameAttribute(AttributeDefaults.OrgAttribute, old.attributes, variantMetadata.attributes) &&
            (hasSameAttribute(AttributeDefaults.BinaryVersionAttribute, old.attributes, variantMetadata.attributes) || //only remove the ones with the same binary attribute
              SemanticVersion.getSemanticVersion(old.attributes).find { case (major, minor, point) => major.toInt == 0 }.isDefined) //remove if old is a semantic version with a prerelease 
        }

      //      val removeArtifactRefs = removeVariants.flatMap(_.configurations.flatMap(_.artifacts))
      //
      //      val artifactFiles = removeArtifactRefs.map(a => ArtifactMetadata.file(adeptGitRepo, a.hash)).toSeq
      val repositoryFiles = removeVariants.map(v => RepositoryMetadata.file(adeptGitRepo, v.id, v.toVariants.map(Hash.calculate(_)))).toSeq
      val variantFiles = removeVariants.map(_.file(adeptGitRepo)).toSeq
      //val repositoryFiles = removeVariants.map(RepositoryMetadata(_.hash, commit))
      //      artifactFiles ++ variantFiles ++ repositoryFiles
      variantFiles ++ repositoryFiles
    }, additions = { content =>
      //      val artifactFiles = result.artifacts.toSeq.map(ArtifactMetadata.fromArtifact(_).write(adeptGitRepo))
      assertCorrectConfigurations()
      val repositoryConfigurationMetadata = commits.map {
        case (configuration, currentCommits) =>
          RepositoryConfiguration(configuration, currentCommits.map { case (id, c) => RepositoryInfo(id, c.repo.name, c.commit) }.toSeq)
      }.toSeq
      val repositoryMetadata = RepositoryMetadata(variantMetadata.id, variantMetadata.toVariants.map(Hash.calculate(_)),
        configurations = repositoryConfigurationMetadata)
      val repositoryFiles = Seq(repositoryMetadata.write(adeptGitRepo))
      val variantFiles = Seq(variantMetadata.write(adeptGitRepo))
      //      artifactFiles ++ variantFiles ++ repositoryFiles
      variantFiles ++ repositoryFiles
    }, commitMsg = "Ivy import of: " + result.mrid)

  }

  def getCommit(mrid: ModuleRevisionId, baseDir: File) = {
    val org = mrid.getOrganisation()
    val repo = org2RepoConversions(org)
    val name = mrid.getName()
    val id = name2IdConversions(name)
    val version = mrid.getRevision()

    def checkVariant(variantMetadata: VariantMetadata): Boolean = {
      val foundVersion = variantMetadata.attributes.find { a =>
        a.name == AttributeDefaults.VersionAttribute && a.values == Set(version)
      }
      val foundName = variantMetadata.attributes.find { a =>
        a.name == AttributeDefaults.NameAttribute && a.values == Set(name)
      }
      val foundOrg = variantMetadata.attributes.find { a =>
        a.name == AttributeDefaults.OrgAttribute && a.values == Set(org)
      }
      val foundPreviousVersioned = foundVersion.isDefined && foundName.isDefined && foundOrg.isDefined
      foundPreviousVersioned
    }

    val gitRepo = new AdeptGitRepository(baseDir, repo)
    val maybeMetadataHit = gitRepo.scanFirst { metadata =>
      metadata.variantMetadata.exists(checkVariant)
    }

    maybeMetadataHit match {
      case Some((adeptCommit, metadata)) =>
        metadata.variantMetadata.filter(checkVariant).map(_.toVariants.map { v =>
          v.id -> adeptCommit
        })
      case _ => Set.empty[Set[(Id, AdeptCommit)]]
    }
  }

  def insert(results: Set[IvyImportResult], baseDir: File) = {
    val gitRepos = Set.empty

    val initialResults = results.filter { result => //TODO: currently this just checks if there is something with the same attributes, but we have to be able to update as well (check artifacts hashes etc etc)
      getCommit(result.mrid, baseDir).isEmpty
    }

    var handledResults: Map[IvyImportResult, Set[(Id, AdeptCommit)]] = Map.empty //TODO: load all results that have been handled allready
    var unhandledResults = initialResults

    var lastSize = -1
    while (unhandledResults.size != lastSize) {
      lastSize = unhandledResults.size

      unhandledResults.foreach { result =>
        val allTransitiveDependencies = {
          def traverseResult(result: IvyImportResult): Set[ModuleRevisionId] = {
            val currentMrids = result.dependencies.flatMap { case (confName, nodes) => nodes.map(_.getId) }.toSet
            currentMrids ++ results.filter(r => currentMrids(r.mrid)).flatMap(traverseResult)
          }
          traverseResult(result)
        }

        val foundCommits = allTransitiveDependencies.flatMap { mrid =>
          getCommit(mrid, baseDir)
        }
        if (allTransitiveDependencies.size == foundCommits.size) {
          //TODO: rewrite this ugly piece of code! what we are doing is essentially finding each commit for each individual configuration
          val configuredCommits: Map[ConfigurationId, Set[(Id, AdeptCommit)]] = result.variantMetadata.configurations.map { configuration =>
            def traverseResult(dependencies: Set[ModuleRevisionId]): Set[ModuleRevisionId] = {
              dependencies ++ results.filter(r => dependencies(r.mrid)).flatMap(r =>
                traverseResult(r.dependencies.flatMap { case (_, nodes) => nodes.map(_.getId) }.toSet))
            }
            configuration.id -> traverseResult(result.dependencies.flatMap {
              case (depConf, _) => result.dependencies.getOrElse(depConf, Set.empty[IvyNode]).map(_.getId)
            }.toSet)
          }.map {
            case (conf, ids) =>
              conf -> ids.flatMap(getCommit(_, baseDir)).flatten
          }.toMap

          handledResults += result -> foundCommits.flatten //TODO: UGLY
          unhandledResults -= result
          updateRepository(baseDir, result, configuredCommits)
        } else None
      }
    }

    if (handledResults.size != initialResults.size) throw new Exception("Could not insert some ivy results: " + unhandledResults.mkString("\n")) //TODO: extract from here  
  }

}

class IvyHelper(ivy: Ivy, changing: Boolean = true, skippableConf: Option[Set[String]] = Some(Set("javadoc", "sources"))) extends Logging {
  import AttributeDefaults.{ NameAttribute, OrgAttribute, VersionAttribute, ArtifactConfAttribute }
  import IvyHelper._

  def createDependencyTree(mrid: ModuleRevisionId) = { //TODO: rename to requirement? or perhaps not?
    var dependencies = Map.empty[ModuleRevisionId, Set[IvyNode]]
    val report = ivy.resolve(mrid, resolveOptions(), changing)
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

  def createIvyResult(mrid: ModuleRevisionId, unloadedChildren: Set[IvyNode]): IvyImportResult = {
    val id = createId(mrid.getName)
    val versionAttribute = Attribute(VersionAttribute, Set(mrid.getRevision()))
    val nameAttribute = Attribute(NameAttribute, Set(mrid.getName()))
    val orgAttribute = Attribute(OrgAttribute, Set(mrid.getOrganisation()))

    val attributes = Set(orgAttribute, nameAttribute, versionAttribute)

    //TODO: replace vars with vals? folding becomes too messy IMO, but it would be more idiomatic?
    var allArtifacts: Set[Artifact] = Set.empty
    var allArtifactFiles: Set[(Artifact, File, String)] = Set.empty
    var configurations = Set.empty[Configuration]
    val dependencyReport = ivy.resolve(mrid, resolveOptions(), changing)
    val moduleDescriptor = dependencyReport.getModuleDescriptor()
    val unloadedChildrenMrid = unloadedChildren.map(_.getId())

    var dependencies = Map.empty[String, Set[IvyNode]]

    moduleDescriptor.getConfigurations().foreach { ivyConfiguration =>
      val confName = ivyConfiguration.getName

      val children = dependencyReport.getDependencies().asScala.flatMap { //we cannot use unloadedChildren directly, because they might not be loaded (if they are provided/eviceted)
        case ivyNode: IvyNode =>
          if (unloadedChildrenMrid(ivyNode.getId)) {
            Some(ivyNode)
          } else None
      }.toSet

      val (loaded, notLoaded) = children.partition(_.isLoaded)

      notLoaded.foreach { ivyNode =>
        if (ivyNode.isEvicted(confName))
          logger.warn(ivyNode + " was not loaded but it is also evicted, so skipping! ") //TODO: is this acceptable? if not find a way to load ivy nodes...
        else
          logger.error(ivyNode + " was not loaded and NOT evicted, so skipping! This is potentially a problem") //TODO: is this acceptable? if not find a way to load ivy nodes...
      }
      val requirements = loaded.map { ivyNode => //evicted nodes are not loaded, we are importing one by one so it is fine
        if (!ivyNode.isEvicted(confName)) {
          val nodes = dependencies.getOrElse(confName, Set.empty) + ivyNode
          dependencies += confName -> nodes //MUTATE
        }

        val requirementId = createId(ivyNode.getId.getName)
        val extraAttributes = moduleDescriptor.getExtraAttributes
        val constraints: Set[Constraint] = extraAttributes.asScala.flatMap {
          case (name: String, value: String) =>
            Some(Constraint(name, Set(value)))
          case _ => None
        }.toSet

        val configurations = {
          val ivyConfigurations = ivyNode.getConfigurations(confName).toSet.map(ivyNode.getConfiguration)
          ivyConfigurations.map(c => ConfigurationId(c.getName))
        }

        ConfiguredRequirement(requirementId, configurations, constraints = constraints)
      }

      val artifactInfos = ivy.resolve(mrid, resolveOptions(ivyConfiguration.getName), changing).getArtifactsReports(mrid).flatMap { artifactReport =>
        val file = artifactReport.getLocalFile
        if (file != null) {
          Some((artifactReport.getArtifactOrigin().getLocation(), artifactReport.getArtifact().getConfigurations(), file, Hash.calculate(file), file.getName))
        } else if (file == null && skippableConf.isDefined && skippableConf.get(ivyConfiguration.getName())) {
          None
        } else {
          throw new Exception("Could not download: " + mrid + " in " + confName)
        }
      }.toSet

      //TODO: skipping empty configurations? if (artifactInfos.nonEmpty || dependencies.nonEmpty)... 
      val currentArtifactFiles = artifactInfos.map {
        case (location, _, file, hash, filename) =>
          (Artifact(hash, file.length, Set(location)), file, filename)
      }

      allArtifactFiles ++= currentArtifactFiles //MUTATE!

      val currentArtifacts = currentArtifactFiles.map(_._1)
      allArtifacts ++= currentArtifacts //MUTATE!

      val artifactRefs = artifactInfos.map {
        case (_, ivyConfs, file, hash, filename) =>
          ArtifactRef(hash, Set(Attribute(ArtifactConfAttribute, ivyConfs.toSet)), Some(filename))
      }

      configurations += Configuration(
        id = ConfigurationId(confName),
        extendsConfigurations = ivyConfiguration.getExtends().map(ConfigurationId(_)).toSet, //FIXME: does not seem to work?
        metadataInfo = Set.empty, //TODO: configuration description?
        artifacts = artifactRefs,
        attributes = attributes,
        requirements = requirements)
    }
    //    case class VariantMetadata(id: Id, metadata: Set[MetadataInfo], attributes: Set[Attribute], configurations: Set[Configuration]) {

    val metadata = VariantMetadata(id,
      metadataInfo = Set.empty,
      attributes = attributes,
      configurations = configurations)

    IvyImportResult(mrid, dependencies, metadata, allArtifacts, allArtifactFiles)
  }

  /**
   * Import from ivy based on coordinates
   */
  def ivyImport(org: String, name: String, version: String): Either[String, Set[IvyImportResult]] = {
    ivy.synchronized { // ivy is not thread safe
      val mrid = ModuleRevisionId.newInstance(org, name, version)
      val dependencyTree = createDependencyTree(mrid)
      val workingNode = dependencyTree(ModuleRevisionId.newInstance(org, name + "-caller", "working")).head.getId
      val result = results(workingNode)(dependencyTree)
      Right(result)
    }
  }

  private def results(mrid: ModuleRevisionId)(dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Set[IvyImportResult] = {
    val children = dependencies.getOrElse(mrid, Set.empty)
    val currentResult = createIvyResult(mrid, children)
    children.flatMap { childNode =>
      val childId = childNode.getId
      val dependencyTree = createDependencyTree(childId)
      results(childId)(dependencies ++ dependencyTree)
    } + currentResult
  }
}