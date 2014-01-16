package adept.ivy

import adept.core.models._
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
import adept.configuration.VariantBuilder
import adept.configuration.ConfiguredDependency
import adept.configuration.ConfigurationId
import adept.configuration.ConfiguredVariantInfo
import adept.repository._
import adept.ext.Version

class IvyResolveException(msg: String) extends RuntimeException(msg)

case class IvyImportResult(mrid: ModuleRevisionId, variants: Set[Variant], artifacts: Set[Artifact], localFiles: Map[Artifact, File])

object IvyHelper {

  def getVersion(variant: Variant): Option[Version] = {
    val versionValues = variant.attribute("version").values
    if (versionValues.size > 1) {
      println("Found multiple versions for: " + variant) //TODO: logger.warn
      None
    } else {
      versionValues.headOption.map(Version.apply _)
    }
  }

  def getRepoName(ivyResult: IvyImportResult) = ivyResult.mrid.getOrganisation

  def insert(results: Set[IvyImportResult], baseDir: File) = {
    val repositories = results.map { ivyResult =>
      val repoName = getRepoName(ivyResult)

      if (!AdeptRepositoryManager.exists(baseDir, repoName)) {

        val repoRef = AdeptRepositoryManager.init(baseDir, repoName)
        if (repoRef.isFailure) throw new Exception("Failed to import ivy to " + baseDir + ". Got errors: " + repoRef.getErrorMessages.mkString(" and "))
      }
      results.flatMap { ivyResult =>
        println("working on " + ivyResult.mrid)
        val repoName = getRepoName(ivyResult)
        val repo = new LocalGitRepository(baseDir, name = repoName, Commit.Head)
        val commitMsg = "Ivy import of: " + ivyResult.mrid
        ivyResult.variants.map { variant =>
          val version = getVersion(variant)
          repo.scan(variant.id) { scannedVariant =>
            val res = for {
              scannedVersion <- getVersion(scannedVariant)
              version <- version
            } yield {
              val a = scannedVersion < version
              println("  " + scannedVariant + " is smaller than " + variant + " " + a)
              a
            }

            res.getOrElse(false)
          } match {
            case Some(commit) =>
              println("wedging: " + variant)
              val revCommit = repo.wedge(variant, commit, commitMsg)
//              new LocalGitRepository(baseDir, repoName, Commit(revCommit.name))
            case None =>
              if (repo.nonEmpty) {
                println("smallest variant yet, wedging after init")
                
                val revCommit = repo.wedge(variant, Commit(Repository.InitTag), commitMsg)
                
//                new LocalGitRepository(baseDir, repoName, Commit(revCommit.name))
              } else {
                println("first variant" + variant)
                repo.writeVariant(variant)
                repo.commit(commitMsg)
              }
          }
        }
      }
    }
  }

  def createId(org: String, name: String) = {
    Id(org + Id.Sep + name)
  }

  def load(path: Option[String] = None, logLevel: Int = Message.MSG_ERR, ivyLogger: AbstractMessageLogger = new DefaultMessageLogger(Message.MSG_ERR)): Either[String, Ivy] = {
    //setting up logging
    Message.setDefaultLogger(ivyLogger)
    val ivy = IvyContext.getContext.getIvy
    val res = path.map { path =>
      val ivySettings = new File(path)
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
      //ivyRoot.foreach(settings.setDefaultIvyUserDir) //FIXME: TODO I do not understand why this does not WORK?!?! Perhaps I didn't well enough?
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
}

class IvyHelper(ivy: Ivy, changing: Boolean = true) {
  import AttributeDefaults.{ NameAttribute, OrgAttribute, VersionAttribute, ConfAttribute }
  import IvyHelper._

  /**
   * Import from ivy based on coordinates
   *
   * TODO: high pri return overrides as well, they are needed to resolve in the same way as Ivy
   */
  def ivyImport(org: String, name: String, version: String): Either[String, Set[IvyImportResult]] = {
    ivy.synchronized { // ivy is not thread safe
      val mrid = ModuleRevisionId.newInstance(org, name, version)
      val dependencies = createDependencyTree(mrid)
      println(dependencies)
      val workingNode = dependencies(ModuleRevisionId.newInstance(org, name + "-caller", "working")).head.getId
      val result = results(workingNode)(dependencies)
      Right(result)
    }
  }

  def createDependencyTree(mrid: ModuleRevisionId) = {
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

  def createIvyResult(mrid: ModuleRevisionId, children: Set[IvyNode]): IvyImportResult = {

    val id = createId(mrid.getOrganisation, mrid.getName)
    val versionAttribute = Attribute(VersionAttribute, Set(mrid.getRevision()))
    val nameAttribute = Attribute(NameAttribute, Set(mrid.getName()))
    val orgAttribute = Attribute(OrgAttribute, Set(mrid.getOrganisation()))

    val attributes = Set(orgAttribute, nameAttribute, versionAttribute)

    //TODO: replace vars with vals? folding becomes too messy IMO, but it would be more idiomatic?
    var allArtifacts: Set[Artifact] = Set.empty
    var allArtifactFiles: Map[Artifact, File] = Map.empty
    var variantBuilder = VariantBuilder.create(id, attributes)

    val moduleDescriptor = ivy.resolve(mrid, resolveOptions(), changing).getModuleDescriptor
    moduleDescriptor.getConfigurations().foreach { ivyConfiguration =>
      val confName = ivyConfiguration.getName

      val dependencies = children.map { ivyNode =>
        val dependencyId = createId(ivyNode.getId.getOrganisation, ivyNode.getId.getName)

        //we do not care if an ivyNode is evicted, we might still possibly need it so
        //no need to check: ivyNode.isEvicted(confName))

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
          val ivyConfigurations = ivyNode.getConfigurations(confName).toSet.map(ivyNode.getConfiguration)
          ivyConfigurations.map(c => ConfigurationId(c.getName))
        }
        ConfiguredDependency(dependencyId, configurations, constraints = constraints)
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
          ArtifactRef(hash, Set(Attribute(ConfAttribute, ivyConfs.toSet)), Some(file.getName))
      }

      variantBuilder = variantBuilder.withConfiguration( //MUTATE!
        artifacts = artifactRefs,
        dependencies = dependencies,
        configuration = ConfigurationId(confName),
        extendsConfigurations = ivyConfiguration.getExtends().map(ConfigurationId(_)).toSet,
        description = ivyConfiguration.getDescription)
    }
    IvyImportResult(mrid, variantBuilder.build(), allArtifacts, allArtifactFiles)
  }

  def results(mrid: ModuleRevisionId)(dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Set[IvyImportResult] = {
    val children = dependencies.getOrElse(mrid, Set.empty)
    val currentResult = createIvyResult(mrid, children)
    children.flatMap { childNode =>
      val childId = childNode.getId
      results(childId)(dependencies ++ createDependencyTree(childId))
    } + currentResult
  }
}