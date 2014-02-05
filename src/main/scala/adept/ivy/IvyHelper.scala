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
  
  
  def insert(results: Set[IvyImportResult], baseDir: File) = {
    logger.warn("NOT IMPLEMENTED")
    results.foreach { result =>
      val repoId = result.mrid.getOrganisation()
      val adeptGitRepo = new AdeptGitRepository(baseDir, repoId)
      
      adeptGitRepo.updateMetadata({ content =>
        content.variantsMetadata.map(_.file(adeptGitRepo)).toSeq
      }, { content =>
        Seq(result.variantsMetadata.write(adeptGitRepo))
      }, "Ivy import of: " + result.mrid)
    }
    
//    var repositories = Set.empty[AdeptCommit]
//    var repositoryMap = Map.empty[ModuleRevisionId, LocalGitRepository]
//    var handledResults = Set.empty[ModuleRevisionId]
//    var unhandledResults = results
//
//    var safeGuard = 0
//    while (handledResults.size < results.size) {
//      safeGuard += 1
//      if (safeGuard == SafeGuardLimit) throw new Exception("Got into loop while inserting from Ivy? Tried to insert: " + SafeGuardLimit + " times, which is set as max")
//
//      val currentResults = unhandledResults.filter { result => //TODO: stop when currentResults is 0 instead of silly safe guard!
//        result.variants.forall { variant =>
//          variant.dependencies.isEmpty || {
//            variant.dependencies.forall { dependency =>
//              if (VariantBuilder.stripConfig(dependency.id) == VariantBuilder.stripConfig(variant.id)) { //TODO: evaluate if this is right - we do it because of the "configuration" dependencies that just depend on the module itself  
//                true
//              } else {
//                handledResults(dependencyAsMrid(dependency)) //we are relying on the fact that we get the complete tree even if the id, values are already present
//              }
//            }
//          }
//        }
//      }
//      val currentRepositories = updateRepositories(results, baseDir) 
//      repositories ++= currentRepositories
//      handledResults ++= currentResults.map(_.mrid)
//      unhandledResults --= currentResults
//    }
//    repositories
  }
}

class IvyHelper(ivy: Ivy, changing: Boolean = true) extends Logging {
  import AttributeDefaults.{ NameAttribute, OrgAttribute, VersionAttribute, ArtifactConfAttribute }
  import IvyHelper._

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

  private val SafeGuardLimit = 1000

  def insert(results: Set[IvyImportResult], baseDir: File) = {
    var commits = Map.empty[ModuleRevisionId, AdeptCommit]
    var handledModules = Set.empty[ModuleRevisionId]
    var unhandledResults = results

    var safeGuard = 0
    while (unhandledResults.size > 0) {
      safeGuard += 1
      if (safeGuard == SafeGuardLimit) throw AdeptIvyException("Got into loop while inserting from Ivy? Tried to insert: " + SafeGuardLimit + " times, which is set as max")

      val currentResults = unhandledResults.filter { result => //TODO: stop when currentResults is 0 instead of silly safe guard!

        true
      }
    }
  }

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
    println(dependencies)
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
    var allArtifactFiles: Map[Artifact, File] = Map.empty
    var configurations = Set.empty[Configuration]
    val dependencyReport = ivy.resolve(mrid, resolveOptions(), changing)
    val moduleDescriptor = dependencyReport.getModuleDescriptor()
    val unloadedChildrenMrid = unloadedChildren.map(_.getId())

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
        val requirementId = createId(ivyNode.getId.getName)

        val extraAttributes = moduleDescriptor.getExtraAttributes
        val constraints: Set[Constraint] = extraAttributes.asScala.flatMap {
          case (name: String, value: String) =>
            Some(Constraint(name, Set(value)))
          case _ => None
        }.toSet ++ Set(
          Constraint(OrgAttribute, Set(ivyNode.getId.getOrganisation)),
          Constraint(NameAttribute, Set(ivyNode.getId.getName)),
          Constraint(VersionAttribute, Set(ivyNode.getId.getRevision)))
        val configurations = {
          println(mrid + " @ " + ivyNode + "   " + confName + " is " + ivyNode.isEvicted(confName))
          val ivyConfigurations = ivyNode.getConfigurations(confName).toSet.map(ivyNode.getConfiguration)
          ivyConfigurations.map(c => ConfigurationId(c.getName))
        }
        ConfiguredRequirement(requirementId, configurations, commits = Set.empty, constraints = constraints)
      }

      val artifactInfos = ivy.resolve(mrid, resolveOptions(ivyConfiguration.getName), changing).getArtifactsReports(mrid).map { artifactReport =>
        val file = artifactReport.getLocalFile
        (artifactReport.getArtifactOrigin().getLocation(), artifactReport.getArtifact().getConfigurations(), file, Hash.calculate(file))
      }.toSet

      //TODO: skipping empty configurations? if (artifactInfos.nonEmpty || dependencies.nonEmpty)... 
      val currentArtifactFiles = artifactInfos.map {
        case (location, _, file, hash) =>
          Artifact(hash, file.length, Set(location)) -> file
      }

      allArtifactFiles ++= currentArtifactFiles //MUTATE!

      val currentArtifacts = currentArtifactFiles.map(_._1)
      allArtifacts ++= currentArtifacts //MUTATE!

      val artifactRefs = artifactInfos.map {
        case (_, ivyConfs, file, hash) =>
          ArtifactRef(hash, Set(Attribute(ArtifactConfAttribute, ivyConfs.toSet)), Some(file.getName))
      }

      configurations += Configuration(
        id = ConfigurationId(confName),
        extendsConfigurations = ivyConfiguration.getExtends().map(ConfigurationId(_)).toSet,
        metadata = Set.empty, //TODO: configuration description?
        artifacts = artifactRefs,
        attributes = attributes,
        requirements = requirements)
    }
    //    case class ConfiguredVariantsMetadata(id: Id, metadata: Set[MetadataInfo], attributes: Set[Attribute], configurations: Set[Configuration]) {

    val metadata = ConfiguredVariantsMetadata(id,
      metadata = Set.empty,
      attributes = attributes,
      configurations = configurations)
    IvyImportResult(mrid, metadata, allArtifacts, allArtifactFiles)
  }

  def results(mrid: ModuleRevisionId)(dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Set[IvyImportResult] = {
    val children = dependencies.getOrElse(mrid, Set.empty)
    val currentResult = createIvyResult(mrid, children)
    children.flatMap { childNode =>
      val childId = childNode.getId
      val dependencyTree = createDependencyTree(childId)
      results(childId)(dependencies ++ dependencyTree)
    } + currentResult
  }
}