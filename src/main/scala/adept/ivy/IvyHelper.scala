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

case class AdeptIvyResolveException(msg: String) extends Exception(msg)
case class AdeptIvyException(msg: String) extends Exception(msg)

object IvyHelper extends Logging {
  import AttributeDefaults.{ NameAttribute, OrgAttribute, VersionAttribute }

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

  def ivyIdAsId(mrid: ModuleRevisionId): Id = {
    Id(mrid.getName)
  }
  val IdConfig = "config"

  def withConfiguration(id: Id, confName: String): Id = {
    Id(id.value + Id.Sep + IdConfig + Id.Sep + confName)
  }
  
  def ivyIdAsId(mrid: ModuleRevisionId, confName: String): Id = {
    assert(!confName.contains(Id.Sep))
    withConfiguration(Id(mrid.getName), confName)
  }

  def ivyIdAsRepositoryName(mrid: ModuleRevisionId): RepositoryName = {
    RepositoryName(mrid.getOrganisation)
  }

  def ivyIdAsVersion(mrid: ModuleRevisionId): Version = {
    Version(mrid.getRevision)
  }
}

case class IvyImportResult(variant: Variant, artifacts: Set[Artifact], localFiles: Map[ArtifactHash, File], repository: RepositoryName, versionInfo: Set[(RepositoryName, Id, Version)])

class IvyHelper(ivy: Ivy, changing: Boolean = true, skippableConf: Option[Set[String]] = Some(Set("javadoc", "sources"))) extends Logging {
  import AttributeDefaults.{ NameAttribute, OrgAttribute, VersionAttribute, ArtifactConfAttribute }
  import IvyHelper._

  val ConfigurationAttribute = "configuration-hash"

  /**
   * Import from ivy based on coordinates
   */
  def ivyImport(org: String, name: String, version: String): Set[IvyImportResult] = {
    val mergableResults = ivy.synchronized { // ivy is not thread safe
      val mrid = ModuleRevisionId.newInstance(org, name, version)
      val dependencyTree = createDependencyTree(mrid)
      val workingNode = dependencyTree(ModuleRevisionId.newInstance(org, name + "-caller", "working")).head.getId
      val result = results(workingNode)(dependencyTree)
      result
    }
    mergableResults
  }

  private def results(mrid: ModuleRevisionId)(dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Set[IvyImportResult] = {
    val children = dependencies.getOrElse(mrid, Set.empty)
    val currentResults = createIvyResult(mrid, children)
    children.flatMap { childNode =>
      val childId = childNode.getId
      val dependencyTree = createDependencyTree(childId)
      results(childId)(dependencies ++ dependencyTree)
    } ++ currentResults
  }

  private def createIvyResult(mrid: ModuleRevisionId, unloadedChildren: Set[IvyNode]): Set[IvyImportResult] = {
    val id = ivyIdAsId(mrid)
    val versionAttribute = Attribute(VersionAttribute, Set(mrid.getRevision()))
    val nameAttribute = Attribute(NameAttribute, Set(mrid.getName()))
    val orgAttribute = Attribute(OrgAttribute, Set(mrid.getOrganisation()))

    val configurationHash = Hasher.hash(mrid.toString.getBytes) //TODO: make more unique? 
    val attributes = Set(orgAttribute, nameAttribute, versionAttribute)

    val dependencyReport = ivy.resolve(mrid, resolveOptions(), changing)
    val moduleDescriptor = dependencyReport.getModuleDescriptor()

    val parentNode = dependencyReport.getDependencies().asScala.map { case i: IvyNode => i }.head

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
        if (ivyNode.isEvicted(confName))
          logger.debug(ivyNode + " was not loaded but it is also evicted, so skipping!")
        else if (ivyNode.getDescriptor().canExclude()) {
          logger.warn(ivyNode + " can be excluded, so skipping!")
        } else {
          logger.error(ivyNode + " was not loaded and NOT evicted, so skipping! This is potentially a problem") //TODO: is this acceptable? if not find a way to load ivy nodes...
        }
      }

      //get requirements:
      val requirements = loaded.flatMap { ivyNode =>
        ivyNode.getConfigurations(confName).toSet.map(ivyNode.getConfiguration).map { requirementConf =>
          Requirement(ivyIdAsId(ivyNode.getId, requirementConf.getName()), Set.empty)
        } + Requirement(ivyIdAsId(ivyNode.getId), Set.empty)
      }

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
        Requirement(ivyIdAsId(mrid, targetConf), Set(Constraint(ConfigurationAttribute, Set(configurationHash))))
      }

      val variant = Variant(
        ivyIdAsId(mrid, confName),
        attributes = attributes,
        artifacts = artifactRefs,
        requirements = requirements)

      val targetVersionInfo = loaded.flatMap { ivyNode =>
        if (!ivyNode.isEvicted(confName)) {
          val targetId = ivyIdAsId(ivyNode.getId)
          val targetRepositoryName = ivyIdAsRepositoryName(ivyNode.getId)
          val targetVersion = ivyIdAsVersion(ivyNode.getId)
          Some((targetRepositoryName, targetId, targetVersion))
        } else {
          None
        }
      }

      IvyImportResult(
        variant = variant,
        artifacts = artifacts,
        localFiles = localFiles,
        repository = ivyIdAsRepositoryName(mrid),
        versionInfo = targetVersionInfo)
    }.toSet

    mergableResults +
      IvyImportResult( //<-- adding main configuration to make sure that there is not 2 variants with different "configurations" 
        variant = Variant(id, attributes = attributes + Attribute(ConfigurationAttribute, Set(configurationHash))),
        artifacts = Set.empty,
        localFiles = Map.empty,
        repository = ivyIdAsRepositoryName(mrid),
        versionInfo = Set.empty)
  }

  private def createDependencyTree(mrid: ModuleRevisionId) = { //TODO: rename to requirement? or perhaps not?
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
}