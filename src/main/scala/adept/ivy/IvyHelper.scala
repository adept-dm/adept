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

class IvyResolveException(msg: String) extends RuntimeException(msg)

case class IvyImportResult(variant: Variant, artifacts: Set[Artifact], localFiles: Map[Artifact, File])

object IvyHelper {
  def convert(org: String, name: String) = {
    Id(org + "/" + name)
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

  val MavenConfs = Set("default", "master", "compile", "provided", "runtime", "test", "system", "sources", "javadoc", "optional")

  def ivyResultsForConf(mrid: ModuleRevisionId, report: ResolveReport, confName: String, children: Set[IvyNode]) = {
    val versionAttribute = Attribute(VersionAttribute, Set(mrid.getRevision()))
    val nameAttribute = Attribute(NameAttribute, Set(mrid.getName()))
    val orgAttribute = Attribute(OrgAttribute, Set(mrid.getOrganisation()))
    val id = convert(mrid.getOrganisation, mrid.getName)

    val attributes = Set(orgAttribute, nameAttribute, versionAttribute, Attribute(ConfAttribute, Set(confName)))

    val dependencies = children.map { ivyNode =>
      val dependencyId = convert(ivyNode.getId.getOrganisation, ivyNode.getId.getName)
      val report = ivy.resolve(ivyNode.getId, resolveOptions(confName), changing)

      //we do not care if an ivyNode is evicted, we might still possibly need it so
      // we do not check if: ivyNode.isEvicted(confName))

      val extraAttributes = report.getModuleDescriptor.getExtraAttributes
      val constraints: Set[Constraint] = extraAttributes.asScala.flatMap {
        case (name: String, value: String) =>
          Some(Constraint(name, Set(value)))
        case _ => None
      }.toSet ++ Set(
        Constraint(OrgAttribute, Set(ivyNode.getId.getOrganisation)),
        Constraint(NameAttribute, Set(ivyNode.getId.getName)),
        Constraint(VersionAttribute, Set(ivyNode.getId.getRevision)))

      Dependency(dependencyId, constraints = constraints)
    }

    val artifactInfos = report.getArtifactsReports(mrid).map { artifactReport =>
      val file = artifactReport.getLocalFile
      (artifactReport.getArtifactOrigin().getLocation(), file, Hash.calculate(file))
    }.toSet

    val artifactFiles = artifactInfos.map {
      case (location, file, hash) =>
        Artifact(hash, file.length, Set(location)) -> file
    }
    val artifactRefs = artifactInfos.map {
      case (_, file, hash) =>
        ArtifactRef(hash, Set(Attribute(ConfAttribute, Set(confName))), Some(file.getName))
    }
    val variant = Variant(id, attributes = attributes, artifacts = artifactRefs, dependencies = dependencies)

    val artifacts = artifactFiles.map(_._1)
    
    IvyImportResult(variant, artifacts, artifactFiles.toMap)
  }

  def createIvyResult(mrid: ModuleRevisionId, children: Set[IvyNode]): Set[IvyImportResult] = {
    val report = ivy.resolve(mrid, resolveOptions(), changing)

    val moduleDescriptor = report.getModuleDescriptor

    val publicConfs = moduleDescriptor.getConfigurations().filter(_.getVisibility() == Visibility.PUBLIC)

    if (publicConfs.map(_.getName).toSet == MavenConfs) {
      //skip compile because it is not used internally in a module
      //skip master because maven imported modules does not use it
      //skip test because it is private and not required to build or use an imported module
      //skip system, optional and provided because it only is defined on the actual dependencies and not in the actual module itself (you can declare a dependency on another lib as optional, but the lib itself is never optional to itself)
      val defaultResult = ivyResultsForConf(mrid, ivy.resolve(mrid, resolveOptions("default"), changing), "default", children)
      val javadocResult = ivyResultsForConf(mrid, ivy.resolve(mrid, resolveOptions("javadoc"), changing), "javadoc", children)
      val sourcesResult = ivyResultsForConf(mrid, ivy.resolve(mrid, resolveOptions("sources"), changing), "sources", children)

      val mainResult = {
        val variant = defaultResult.variant.copy(artifacts = defaultResult.variant.artifacts ++ javadocResult.variant.artifacts ++ sourcesResult.variant.artifacts)
        defaultResult.copy(variant = variant, artifacts = defaultResult.artifacts ++ javadocResult.artifacts ++ sourcesResult.artifacts)
      }

      val runtimeResult = ivyResultsForConf(mrid, ivy.resolve(mrid, resolveOptions("runtime"), changing), "runtime", children)

      if (runtimeResult.variant.id == mainResult.variant.id &&
        runtimeResult.variant.attributes.filter(a => a.name != ConfAttribute) == mainResult.variant.attributes.filter(a => a.name != ConfAttribute) &&
        runtimeResult.variant.dependencies == mainResult.variant.dependencies) {
        val attributes = mainResult.variant.attributes.filter(a => a.name != ConfAttribute) + Attribute(ConfAttribute, Set("compile", "runtime"))

        //merge runtime if it is the same as compile:
        val resultVariant = mainResult.variant.copy(attributes = attributes,
          artifacts = mainResult.variant.artifacts ++ runtimeResult.variant.artifacts)
        val resultArtifacts = mainResult.artifacts ++ runtimeResult.artifacts
        val resultArtifactFiles = mainResult.localFiles ++ runtimeResult.localFiles

        Set(IvyImportResult(resultVariant, resultArtifacts, resultArtifactFiles))
      } else {
        Set(mainResult, runtimeResult)
      }
    } else {
      publicConfs.flatMap { conf =>
        val result = ivyResultsForConf(mrid, report, conf.getName, children)
        if (result.variant.dependencies.nonEmpty && result.variant.artifacts.nonEmpty)
          Some(result)
        else None
      }.toSet
    }
  }

  def results(mrid: ModuleRevisionId)(dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Set[IvyImportResult] = {
    val children = dependencies.getOrElse(mrid, Set.empty)
    val currentVariants = createIvyResult(mrid, children).toSet
    children.flatMap { childNode =>
      val childId = childNode.getId
      results(childId)(dependencies ++ createDependencyTree(childId))
    } ++ currentVariants
  }
}