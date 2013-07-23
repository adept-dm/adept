package adept.ivy

import adept.core.models._
import java.io.{ File => jFile }
import org.apache.ivy._
import org.apache.ivy.core._
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve._
import org.apache.ivy.core.retrieve.RetrieveOptions
import org.apache.ivy.core.report.ResolveReport
import scala.util._
import java.io.File
import adept.core.Adept
import org.apache.ivy.util.Message
import org.apache.ivy.util.MessageLogger
import org.apache.ivy.util.DefaultMessageLogger
import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import adept.utils.Logging
import collection.JavaConverters._
import org.apache.ivy.core.report.ResolveReport
import org.apache.ivy.core.module.descriptor.{ Artifact => IvyArtifact }
import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }
import org.apache.ivy.plugins.resolver.util.ResolverHelper
import org.apache.ivy.plugins.resolver.DependencyResolver
import org.apache.ivy.plugins.resolver.RepositoryResolver
import org.apache.ivy.core.search.ModuleEntry
import org.apache.ivy.core.search.OrganisationEntry
import org.apache.ivy.plugins.repository.Repository
import org.apache.ivy.plugins.version.PatternVersionMatcher
import org.apache.ivy.core.module.descriptor.OverrideDependencyDescriptorMediator
import org.apache.ivy.plugins.matcher.Matcher
import org.apache.ivy.plugins.matcher.MapMatcher
import org.apache.ivy.plugins.matcher.PatternMatcher

case class ExpectedResolveException(msg: String) extends Exception(msg)

object IvyHelpers extends Logging {

  def load(path: Option[String] = None, logLevel: Int = Message.MSG_ERR): Either[String, Ivy] = {
    //setting up logging
    val ivyLogger = new DefaultMessageLogger(logLevel)
    //ivyLogger.setShowProgress(false);
    //Message.setDefaultLogger(ivyLogger)
    val ivy = IvyContext.getContext.getIvy
    val res = path.map { path =>
      val ivySettings = new jFile(path)
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
      //ivyRoot.foreach(settings.setDefaultIvyUserDir) //FIXME: TODO this does not WORK?!?!
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

  def confString(modConf: String, depConfs: Array[String]) = {
    modConf + "->" + depConfs.mkString(",")
  }
  private val changing = true

  private def parent(report: ResolveReport): IvyNode = {
    report.getDependencies().asScala.map { case i: IvyNode => i }.head
  }

  private def adeptConfiguration(parent: IvyNode, cachedConf: IvyConfiguration): Configuration = {
    val c = parent.getConfiguration(cachedConf.getName)
    Configuration(
      name = c.getName(),
      description = Option(c.getDescription()),
      extendsFrom = c.getExtends().toSet,
      visibility = c.getVisibility match {
        case c if c == IvyConfiguration.Visibility.PUBLIC => Visibility.Public
        case c if c == IvyConfiguration.Visibility.PRIVATE => Visibility.Private
        case somethingElse => throw new Exception("Got unexpected visibility: " + somethingElse)
      },
      deprecated = Option(c.getDeprecated()))
  }

  def adeptArtifacts(ivyArtifacts: Array[IvyArtifact], parentNode: IvyNode, ivy: Ivy) = {
    val artifacts = ivyArtifacts.toList.flatMap {
      case a: IvyArtifact =>
        val artifactReports = ivy.resolve(parentNode.getId(), resolveOptions(a.getConfigurations().toList: _*), changing).getAllArtifactsReports()
        val thisModuleArtifactReports = artifactReports.filter(_.getArtifact().getId() == a.getId())
        thisModuleArtifactReports.map { r =>
          val artifact = r.getArtifact()
          if (r.getArtifactOrigin() == null) throw new ExpectedResolveException("could not find the location for the artifact: " + artifact + " from parent: " + parentNode)
          val location = r.getArtifactOrigin().getLocation()
          val file = r.getLocalFile()
          val artifactType = artifact.getType()
          (file, location, artifactType) -> artifact.getConfigurations().toList
        }
    }.toSet
    artifacts.groupBy(_._1).flatMap {
      case ((file, location, artifactType), all) =>
        if (file != null && file.exists) {
          val confs = all.flatMap { case (_, c) => c }
          Set(Artifact.fromFile(file, artifactType, confs, Set(location)))
        } else throw new Exception("could not find: " + file + " for " + parentNode)
    }.toSet
  }
  private def adeptDependencies(depNodes: Set[(IvyNode, Configuration)], parentNode: IvyNode, ivy: Ivy, findModule: Adept.FindModule, add: Module => Unit)(allCoords: collection.mutable.Set[Coordinates], modules: collection.mutable.Set[Module])(depth: Int): Set[Dependency] = {
    val allCoordinates = depNodes.map {
      case (node, conf) =>
        val coords = resolveDynamicVersion(Coordinates(node.getId.getOrganisation(), node.getId.getName, node.getId.getRevision()), ivy)

        val depReport = ivy.resolve(ModuleRevisionId.newInstance(coords.org, coords.name, coords.version), resolveOptions(), changing) //make sure this artifact has been resolved
        (node, coords, conf)
    }
    val allDependencies = for {
      (node, coords, conf) <- allCoordinates if !allCoords.contains(coords) && (node != parentNode)
    } yield {
      logger.info(allCoords.size + " found so far. " + coords + " has not yet been found")
      Option(node.getDependencyDescriptor(parentNode)).map { depDescriptor =>
        if (depDescriptor.getAllIncludeRules() != null && depDescriptor.getAllIncludeRules().nonEmpty) throw new Exception("Include rules are not implemented") //TODO: <- fix this

        val exclusionRules = depDescriptor.getAllExcludeRules().map { ivyExcludeRule =>
          val org = ivyExcludeRule.getId().getModuleId().getOrganisation()
          val name = ivyExcludeRule.getId().getModuleId().getName()
          DependencyExclusionRule(org, name)
        }.toSet
        val modConfs = depDescriptor.getModuleConfigurations
        val configuration = modConfs.map { modConf =>
          val depConfs = depDescriptor.getDependencyConfigurations(modConf)
          confString(modConf, depConfs)
        }.mkString(";")

        allCoords += coords
        logger.info("adding " + coords + " to allCoords")
        adeptModule(coords, ivy, findModule, add)(allCoords, modules)(depth + 1) match { //TODO: @tailrec (this recurses because adeptModule calls adeptDependencies and apdetDependencies calls adeptModule
          case Right(module) =>
            val uniqueId = module.uniqueId
            Dependency(coords, Some(uniqueId), configuration, force = depDescriptor.isForce(), isTransitive = depDescriptor.isTransitive(), exclusionRules = exclusionRules)
          case Left(msg) =>
            logger.trace("could resolve module: " + msg)
            if (mustBeAvailable(conf)) {
              logger.error("could not find required module: " + coords + "." + msg)
            } //else throw new Exception("could not find required module: " + coords + ". " + msg)
            Dependency(coords, None, configuration, force = depDescriptor.isForce(), isTransitive = depDescriptor.isTransitive(), exclusionRules = exclusionRules)
        }
      }
    }
    allDependencies.flatten
  }

  private def mustBeAvailable(conf: Configuration): Boolean = {
    (conf.name.toLowerCase match {
      case "optional" => false
      case "system" => false
      case "provided" => false
      case _ => true
    }) && (
      conf.visibility == Visibility.Public)
  }

  private def resolveDynamicVersion(initCoords: Coordinates, ivy: Ivy): Coordinates = {
    val versionMatcher = ivy.getResolveEngine().getSettings().getVersionMatcher()
    val isDynamic = versionMatcher.isDynamic(ModuleRevisionId.newInstance(initCoords.org, initCoords.name, initCoords.version))

    if (isDynamic) {
      val all = ivy.getSettings().getResolvers.asScala.flatMap {
        case resolver: DependencyResolver =>
          resolver.listRevisions(new ModuleEntry(new OrganisationEntry(resolver, initCoords.org), initCoords.name)).map {
            _.getRevision
          }.filter { version =>
            versionMatcher.accept(ModuleRevisionId.newInstance(initCoords.org, initCoords.name, initCoords.version),
              ModuleRevisionId.newInstance(initCoords.org, initCoords.name, version))
          }
        case other =>
          logger.warn("skipping dynamic version this resolver on: " + initCoords + " because found " + other.getClass + " and not DependencyResolver")
          Set.empty
      }

      val latestVersion = all.reduce { (last, current) =>
        val lastVersion = ModuleRevisionId.newInstance(initCoords.org, initCoords.name, last)
        val currentVersion = ModuleRevisionId.newInstance(initCoords.org, initCoords.name, current)

        if (versionMatcher.accept(lastVersion, currentVersion)) {
          lastVersion.getRevision()
        } else currentVersion.getRevision()
      }

      initCoords.copy(version = latestVersion)
    } else {
      initCoords
    }
  }

  private def adeptOverrides(parentNode: IvyNode, ivy: Ivy, findModule: Adept.FindModule, add: Module => Unit)(allCoords: collection.mutable.Set[Coordinates], modules: collection.mutable.Set[Module])(depth: Int): Set[Override] = {
    parentNode.getDescriptor().getAllDependencyDescriptorMediators().getAllRules().asScala.map {
      case (matcher: MapMatcher, overrideMediator: OverrideDependencyDescriptorMediator) =>
        val matcherName = matcher.getPatternMatcher().getName()
        matcherName match {
          case PatternMatcher.EXACT => true
          case _ => throw new Exception("found matcher: " + matcherName + ". currently only: " + PatternMatcher.EXACT + " is supported. parent:" + parentNode)
        }
        val org = matcher.getAttributes.get(IvyPatternHelper.ORGANISATION_KEY) match { case s: String => s }
        val name = matcher.getAttributes.get(IvyPatternHelper.MODULE_KEY) match { case s: String => s }
        val overriddenVersion = overrideMediator.getVersion()

        val coords = Coordinates(org, name, overriddenVersion)
        if (!allCoords.contains(coords)) {
          allCoords += coords
          logger.info("adding override " + coords + " to allCoords")
          adeptModule(coords, ivy, findModule, add)(allCoords, modules)(depth + 1) match { //TODO: @tailrec (this recurses because adeptModule calls adeptDependencies and apdetDependencies calls adeptModule
            case Right(module) =>
              val uniqueId = module.uniqueId
              Override(coords.org, coords.name, coords.version, Some(uniqueId))
            case Left(msg) =>
              logger.trace("could resolve module: " + msg)
              Override(coords.org, coords.name, coords.version, None)
          }
        } else {
          modules.find(_.coordinates == coords) match {
            case Some(module) =>
              val uniqueId = module.uniqueId
              Override(coords.org, coords.name, coords.version, Some(uniqueId))
            case None =>
              Override(coords.org, coords.name, coords.version, None)
          }
        }
    }.toSet
  }

  val MAX_DEPTH = -1

  def adeptModule(initCoords: Coordinates, ivy: Ivy, findModule: Adept.FindModule, add: Module => Unit)(allCoords: collection.mutable.Set[Coordinates], modules: collection.mutable.Set[Module])(depth: Int): Either[String, Module] = {
    try {
      if (depth < MAX_DEPTH) {
        Left("max depth found")
      } else {
        val coords = resolveDynamicVersion(initCoords, ivy)
        val report = ivy.resolve(ModuleRevisionId.newInstance(coords.org, coords.name, coords.version), resolveOptions(), changing)
        val moduleDescriptor = report.getModuleDescriptor()
        allCoords += coords
        val parentNode = parent(report)
        val isLoaded = parentNode.getDescriptor != null

        if (isLoaded) {
          val configurations = moduleDescriptor.getConfigurations()
            .map(adeptConfiguration(parentNode, _)).toSet

          val attributes: Map[String, Seq[String]] = Map.empty ++
            Option(moduleDescriptor.getDescription()).filter(_.nonEmpty).map(a => "description" -> Seq(a)).toMap ++
            Option(moduleDescriptor.getHomePage()).map(a => "home-page" -> Seq(a)).toMap

          val artifacts = adeptArtifacts(parentNode.getAllArtifacts, parentNode, ivy)

          val uniqueId = {
            Option(moduleDescriptor.getPublicationDate()).map { created =>
              UniqueId.default(coords, created, artifacts)
            }.getOrElse {
              UniqueId.default(coords, artifacts)
            }
          }

          val foundModule: Option[Module] =
            findModule(coords, Some(uniqueId)) match {
              case Right(res) => {
                res //<-- should be res, set to None to refresh. TODO: make configurable
              }
              case _ => None
            }

          val adeptModule = foundModule.getOrElse {

            val depNodes = configurations.flatMap { conf =>
              //TODO: will this works for all confs? I am not sure how to get every possible conf 
              parentNode.getDependencies(conf.name, conf.name, "*").asScala.map { case i: IvyNode => i -> conf }
            }
            val deps = adeptDependencies(depNodes, parentNode, ivy, findModule, add)(allCoords, modules)(depth)
            val overrides = adeptOverrides(parentNode, ivy, findModule, add)(allCoords, modules)(depth)

            val adeptModule = Module(coords, uniqueId, artifacts, configurations, attributes, deps, overrides)
            modules += adeptModule
            synchronized {
              findModule(adeptModule.coordinates, None) match {
                case Right(None) => add(adeptModule)
                case _ => //do nothing
              }
              
            }
            adeptModule
          }

          Right(adeptModule)
        } else {
          logger.info("could not load: " + coords + ". problem messages: " + report.getAllProblemMessages().asScala.mkString("\n"))
          Left("could not load: " + coords + ". problem messages: " + report.getAllProblemMessages().asScala.mkString("\n"))
        }
      }
    } catch {
      case ExpectedResolveException(msg) => {
        Left(msg)
      }
      case exception: Exception => {
        logger.error("could not resolve module:" + initCoords, exception)
        Left(exception.getMessage)
      }
    }
  }

  /** returns the modules that where added */
  def add(coords: Coordinates, ivy: Ivy, adept: Adept): Set[Module] = {
    logger.trace("building dependency tree from ivy...")
    //TODO: mutable collection is perhaps not ideal, then again ivy is not thread safe either way...
    val allCoords = new collection.mutable.HashSet[Coordinates] with collection.mutable.SynchronizedSet[Coordinates]
    val modules = new collection.mutable.HashSet[Module] with collection.mutable.SynchronizedSet[Module]
    adeptModule(coords, ivy, adept.findModule _, adept.add _)(allCoords, modules)(0)
    modules.toSet
  }
}
