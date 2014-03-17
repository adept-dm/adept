//package adept.ivy
//
//import adept.ext.AttributeDefaults
//import org.apache.ivy.Ivy
//import org.apache.ivy.core.module.id.ModuleRevisionId
//import org.apache.ivy.core.resolve.ResolveOptions
//import org.apache.ivy.core.module.descriptor.{ Artifact => IvyArtifact }
//import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }
//import org.apache.ivy.util.Message
//import org.apache.ivy.util.DefaultMessageLogger
//import org.apache.ivy.core.IvyContext
//import java.io.File
//import org.apache.ivy.core.resolve.IvyNode
//import collection.JavaConverters._
//import org.apache.ivy.core.module.descriptor.Configuration.Visibility
//import org.apache.ivy.core.report.ResolveReport
//import org.apache.ivy.util.AbstractMessageLogger
//import adept.ext.Version
//import adept.logging.Logging
//import adept.artifact.models._
//import adept.repository.models._
//import adept.repository.models.configuration._
//import adept.resolution.models._
//
//case class AdeptIvyResolveException(msg: String) extends Exception(msg)
//case class AdeptIvyException(msg: String) extends Exception(msg)
//
//object IvyHelper extends Logging {
//  import AttributeDefaults.{ NameAttribute, OrgAttribute, VersionAttribute }
//
//  def createId(name: String) = {
//    Id(name)
//  }
//
//  lazy val errorIvyLogger = new DefaultMessageLogger(Message.MSG_ERR)
//  lazy val warnIvyLogger = new DefaultMessageLogger(Message.MSG_WARN)
//  lazy val infoIvyLogger = new DefaultMessageLogger(Message.MSG_INFO)
//  lazy val debugIvyLogger = new DefaultMessageLogger(Message.MSG_DEBUG)
//
//  def load(path: Option[String] = None, ivyLogger: AbstractMessageLogger = errorIvyLogger): Ivy = {
//    //setting up logging
//    Message.setDefaultLogger(ivyLogger)
//    val ivy = IvyContext.getContext.getIvy
//    val loadedIvy = path.map { path =>
//      val ivySettings = new File(path)
//      if (!ivySettings.isFile) {
//        throw AdeptIvyException(ivySettings + " is not a file")
//      } else {
//        ivy.configure(ivySettings)
//        ivy
//      }
//    }.getOrElse {
//      ivy.configureDefault()
//      ivy
//    }
//
//    val settings = loadedIvy.getSettings()
//    //ivyRoot.foreach(settings.setDefaultIvyUserDir) //FIXME: TODO I do not understand why this does not WORK?!?! Perhaps I didn't well enough?
//    loadedIvy.setSettings(settings)
//    loadedIvy
//  }
//
//  def resolveOptions(confs: String*) = {
//    val resolveOptions = new ResolveOptions()
//    if (confs.nonEmpty) resolveOptions.setConfs(confs.toArray)
//    resolveOptions.setCheckIfChanged(true)
//    resolveOptions.setRefresh(true)
//    resolveOptions.setDownload(true)
//    resolveOptions.setOutputReport(false) //TODO: to true?
//    resolveOptions
//  }
//
//}
//
//class IvyHelper(ivy: Ivy, changing: Boolean = true, skippableConf: Option[Set[String]] = Some(Set("javadoc", "sources"))) extends Logging {
//  import AttributeDefaults.{ NameAttribute, OrgAttribute, VersionAttribute, ArtifactConfAttribute }
//  import IvyHelper._
//
//  def createDependencyTree(mrid: ModuleRevisionId) = { //TODO: rename to requirement? or perhaps not?
//    var dependencies = Map.empty[ModuleRevisionId, Set[IvyNode]]
//    val report = ivy.resolve(mrid, resolveOptions(), changing)
//    def addDependency(mrid: ModuleRevisionId, ivyNode: IvyNode) = {
//      val current = dependencies.getOrElse(mrid, Set.empty) + ivyNode
//      dependencies += mrid -> current
//    }
//
//    report.getDependencies().asScala.foreach {
//      case ivyNode: IvyNode =>
//        if (mrid != ivyNode.getId) addDependency(mrid, ivyNode)
//    }
//
//    val currentCallers = report.getDependencies().asScala.foreach {
//      case ivyNode: IvyNode => ivyNode.getAllCallers.map { caller =>
//        if (caller.getModuleRevisionId != ivyNode.getId) addDependency(caller.getModuleRevisionId, ivyNode)
//      }
//    }
//    dependencies
//  }
//
//  def createIvyResult(mrid: ModuleRevisionId, unloadedChildren: Set[IvyNode]) = { //: IvyImportResult = {
//    val id = createId(mrid.getName)
//    val versionAttribute = Attribute(VersionAttribute, Set(mrid.getRevision()))
//    val nameAttribute = Attribute(NameAttribute, Set(mrid.getName()))
//    val orgAttribute = Attribute(OrgAttribute, Set(mrid.getOrganisation()))
//
//    val attributes = Set(orgAttribute, nameAttribute, versionAttribute)
//
//    //TODO: replace vars with vals? folding becomes too messy IMO, but it would be more idiomatic?
//    var allArtifacts: Set[Artifact] = Set.empty
//    var allArtifactFiles: Set[(Artifact, File, String)] = Set.empty
//    val dependencyReport = ivy.resolve(mrid, resolveOptions(), changing)
//    val moduleDescriptor = dependencyReport.getModuleDescriptor()
//    val unloadedChildrenMrid = unloadedChildren.map(_.getId())
//
//    val parentNode = dependencyReport.getDependencies().asScala.map { case i: IvyNode => i }.head
//    
//    var dependencies = Map.empty[String, Set[IvyNode]]
//
//    moduleDescriptor.getConfigurations().foreach { ivyConfiguration =>
//      val confName = ivyConfiguration.getName
//
//      val children = dependencyReport.getDependencies().asScala.flatMap { //we cannot use unloadedChildren directly, because they might not be loaded (if they are provided/eviceted)
//        case ivyNode: IvyNode =>
//          if (unloadedChildrenMrid(ivyNode.getId)) {
//            Some(ivyNode)
//          } else None
//      }.toSet
//
//      val (loaded, notLoaded) = children.partition(_.isLoaded)
//
//      notLoaded.foreach { ivyNode =>
//        if (ivyNode.isEvicted(confName))
//          logger.warn(ivyNode + " was not loaded but it is also evicted, so skipping! ") //TODO: is this acceptable? if not find a way to load ivy nodes...
//        else
//          logger.error(ivyNode + " was not loaded and NOT evicted, so skipping! This is potentially a problem") //TODO: is this acceptable? if not find a way to load ivy nodes...
//      }
//      val requirements = loaded.map { ivyNode => //evicted nodes are not loaded, we are importing one by one so it is fine
//        if (!ivyNode.isEvicted(confName)) {
//          val nodes = dependencies.getOrElse(confName, Set.empty) + ivyNode
//          dependencies += confName -> nodes //MUTATE
//        }
//
//        val requirementId = createId(ivyNode.getId.getName)
//        val extraAttributes = moduleDescriptor.getExtraAttributes
//        val constraints: Set[Constraint] = extraAttributes.asScala.flatMap {
//          case (name: String, value: String) =>
//            Some(Constraint(name, Set(value)))
//          case _ => None
//        }.toSet
//
//        val configurations = {
//          val ivyConfigurations = ivyNode.getConfigurations(confName).toSet.map(ivyNode.getConfiguration)
//          ivyConfigurations.map(c => ConfigurationId(c.getName))
//        }
//
//        ConfiguredRequirement(requirementId, configurations, constraints = constraints)
//      }
//
//      val artifactInfos = ivy.resolve(mrid, resolveOptions(ivyConfiguration.getName), changing).getArtifactsReports(mrid).flatMap { artifactReport =>
//        val file = artifactReport.getLocalFile
//        if (file != null) {
//          Some((artifactReport.getArtifactOrigin().getLocation(), artifactReport.getArtifact().getConfigurations(), file, Hash.calculate(file), file.getName))
//        } else if (file == null && skippableConf.isDefined && skippableConf.get(ivyConfiguration.getName())) {
//          None
//        } else {
//          throw new Exception("Could not download: " + mrid + " in " + confName)
//        }
//      }.toSet
//
//      //TODO: skipping empty configurations? if (artifactInfos.nonEmpty || dependencies.nonEmpty)... 
//      val currentArtifactFiles = artifactInfos.map {
//        case (location, _, file, hash, filename) =>
//          (Artifact(hash, file.length, Set(location)), file, filename)
//      }
//
//      allArtifactFiles ++= currentArtifactFiles //MUTATE!
//
//      val currentArtifacts = currentArtifactFiles.map(_._1)
//      allArtifacts ++= currentArtifacts //MUTATE!
//
//      val artifactRefs = artifactInfos.map {
//        case (_, ivyConfs, file, hash, filename) =>
//          ArtifactRef(hash, Set(Attribute(ArtifactConfAttribute, ivyConfs.toSet)), Some(filename))
//      }
//      configurations += Configuration(
//        id = ConfigurationId(confName),
//        extendsConfigurations = parentNode.getConfiguration(confName).getExtends().map(ConfigurationId(_)).toSet,
//        metadataInfo = Set.empty, //TODO: configuration description?
//        artifacts = artifactRefs,
//        attributes = attributes,
//        requirements = requirements)
//    }
//    //    case class VariantMetadata(id: Id, metadata: Set[MetadataInfo], attributes: Set[Attribute], configurations: Set[Configuration]) {
//
//    val metadata = VariantMetadata(id,
//      metadataInfo = Set.empty,
//      attributes = attributes,
//      configurations = configurations)
//
//    IvyImportResult(mrid, dependencies, metadata, allArtifacts, allArtifactFiles)
//  }
//
//  /**
//   * Import from ivy based on coordinates
//   */
//  def ivyImport(org: String, name: String, version: String): Either[String, Set[IvyImportResult]] = {
//    ivy.synchronized { // ivy is not thread safe
//      val mrid = ModuleRevisionId.newInstance(org, name, version)
//      val dependencyTree = createDependencyTree(mrid)
//      val workingNode = dependencyTree(ModuleRevisionId.newInstance(org, name + "-caller", "working")).head.getId
//      val result = results(workingNode)(dependencyTree)
//      Right(result)
//    }
//  }
//
//  private def results(mrid: ModuleRevisionId)(dependencies: Map[ModuleRevisionId, Set[IvyNode]]): Set[IvyImportResult] = {
//    val children = dependencies.getOrElse(mrid, Set.empty)
//    val currentResult = createIvyResult(mrid, children)
//    children.flatMap { childNode =>
//      val childId = childNode.getId
//      val dependencyTree = createDependencyTree(childId)
//      results(childId)(dependencies ++ dependencyTree)
//    } + currentResult
//  }
//}