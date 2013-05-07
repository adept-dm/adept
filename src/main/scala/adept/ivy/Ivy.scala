package adept.ivy


import adept.models._
import java.io.{File => jFile}
import org.apache.ivy._
import org.apache.ivy.core._
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve._
import org.apache.ivy.core.retrieve.RetrieveOptions
import org.apache.ivy.core.report.ResolveReport
import scala.util._
import java.io.File
import org.slf4j.LoggerFactory
import adept.Adept
import org.apache.ivy.util.Message
import org.apache.ivy.util.MessageLogger
import org.apache.ivy.util.DefaultMessageLogger
import org.apache.ivy.core.module.descriptor.ModuleDescriptor

object IvyHelpers {
  protected val logger = LoggerFactory.getLogger(this.getClass)
 
  def load(path: Option[String] = None, logLevel: Int = Message.MSG_ERR): Either[String, Ivy] = {
    //setting up logging
    val ivyLogger = new DefaultMessageLogger(logLevel)
    //ivyLogger.setShowProgress(false);
    //Message.setDefaultLogger(ivyLogger)
    
    val ivy = IvyContext.getContext.getIvy
    val res = path.map{ path =>
      val ivySettings = new jFile(path)
      if (!ivySettings.isFile) {
        Left(ivySettings +" is not a file")
      } else {
        ivy.configure(ivySettings)
        Right(ivy)
      }
    }.getOrElse {
      ivy.configureDefault()
      Right(ivy)
    }
    res.right.map{ ivy =>
      val settings = ivy.getSettings()
      //ivyRoot.foreach(settings.setDefaultIvyUserDir) //FIXME: TODO this does not WORK?!?!
      ivy.setSettings(settings)
      ivy
    }
  }
  
  case class IvyTree(node: IvyNode, children: Seq[IvyTree])
  
  private[ivy] def tree(report: ResolveReport, moduleRevisionId: ModuleRevisionId): Seq[IvyTree] = {
    import collection.JavaConverters._
    val nodes = report.getDependencies().asScala.map(_.asInstanceOf[IvyNode]).filter(!_.isCompletelyEvicted())
    val dependencies: Map[ModuleRevisionId, Seq[IvyNode]] = {
      //TODO: this code was ported from java and is mutable - consider revising
      val mutableDeps = collection.mutable.Map[ModuleRevisionId, collection.mutable.ListBuffer[IvyNode]]()
      nodes.foreach{ ivyNode =>
        if (!mutableDeps.isDefinedAt(ivyNode.getId)) mutableDeps += (ivyNode.getId -> collection.mutable.ListBuffer())
        ivyNode.getAllCallers.foreach{ c =>
          if (!mutableDeps.isDefinedAt(c.getModuleRevisionId)) mutableDeps += (c.getModuleRevisionId -> collection.mutable.ListBuffer())
          val deps = mutableDeps.getOrElse(c.getModuleRevisionId, collection.mutable.ListBuffer())
          deps += ivyNode
        }
      }
      mutableDeps.map{ case (id, deps) => id -> deps.toSeq }.toMap
    }
    def tree(ivyNodes: Seq[IvyNode]): Seq[IvyTree]= {
      ivyNodes.map{ ivyNode =>
        val children = dependencies(ivyNode.getId)
        IvyTree(ivyNode, children = tree(children))
      }
    }
    tree(dependencies(report.getModuleDescriptor.getModuleRevisionId))
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

  def adeptModule(coords: Coordinates, ivy: Ivy): Seq[Module] = {
    import collection.JavaConverters._
    import org.apache.ivy.core.report.ResolveReport
    import org.apache.ivy.core.module.descriptor.{ Artifact => IvyArtifact }
    import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }
    
    def parent(report: ResolveReport): IvyNode = {
       report.getDependencies().asScala.map{ case i: IvyNode => i }.head
    }
    
    val module = ModuleRevisionId.newInstance(coords.org, coords.name, coords.version)
    val report = ivy.resolve(module, resolveOptions(), changing)
    val resolveEngine = ivy.getResolveEngine()
    
    val depNodes = report.getDependencies().asScala.map{ case i: IvyNode => i }
    
    val moduleDescriptor = report.getModuleDescriptor()
    
    val configurations = moduleDescriptor.getConfigurations().toSet.map{ c:IvyConfiguration =>
      Configuration(
          name = c.getName(),
          description = Option(c.getDescription()),
          extendsFrom = c.getExtends().toSet,
          /*TODO: visibility = c.getVisibility match { 
            case c if c == IvyConfiguration.Visibility.PUBLIC=> Visibility.Public
            case c if c == IvyConfiguration.Visibility.PRIVATE => Visibility.Private
            case somethingElse => throw new Exception("Got unexpected visibility: " + somethingElse)
          },*/
          deprecated = Option(c.getDeprecated())
      )
    }
    
    val attributes: Map[String, Seq[String]] = Map.empty ++ 
        Option(moduleDescriptor.getDescription()).map(a => "description"->Seq(a)).toMap ++
        Option(moduleDescriptor.getHomePage()).map(a => "home-page"->Seq(a)).toMap
    
    val parentNode = parent(report)
    val artifacts = {
      val artifacts = configurations.flatMap{ c =>
        parentNode.getArtifacts(c.name).toList.flatMap{ case a: IvyArtifact =>
          ivy.resolve(module, resolveOptions(c.name), changing).getAllArtifactsReports().map{ r =>
            val artifact = r.getArtifact()
            val location = resolveEngine.locate(artifact).getLocation()
            val file = r.getLocalFile()
            val artifactType = artifact.getType() 
            (file, location, artifactType) -> artifact.getConfigurations().toList
          }
        }
      }.toSet
      artifacts.groupBy(_._1).flatMap{ case ((file, location, artifactType), all) => 
        if (file != null && file.exists)
          Set(Artifact.fromFile(file, artifactType, all.flatMap{case (_, c) => c}, Set(location)))
        else Set.empty[Artifact]
      }
    }.toSet
    
    val ivyArtifacts = report.getAllArtifactsReports.toList.map( r => r.getArtifact() -> (r.getLocalFile(), resolveEngine.locate(r.getArtifact()).getLocation())).toMap
    val deps = depNodes.map{ node => node -> node.getDependencyDescriptor(parentNode) }.filter{ case (node, depDescriptor) => depDescriptor != null }.flatMap{ case (node, depDescriptor) =>
      val org = node.getId.getOrganisation()
      val name = node.getId.getName()
      val version = node.getId.getRevision()
      val coords = Coordinates(org, name, version)
      
      val depReport = ivy.resolve(ModuleRevisionId.newInstance(coords.org, coords.name, coords.version), resolveOptions(), changing) //make sure this artifact has been resolved 
      
      val maybeJarFile = {
        if (node.isLoaded) {
          val jarFiles = parent(depReport).getAllArtifacts().toList.filter(a => ivyArtifacts.contains(a)).map(a => ivyArtifacts(a)._1)
          if (jarFiles.size != 1) throw new Exception("while getting artifact for " + node + " - did not get exactly one: " + jarFiles  + ". parent: " + parentNode)
          jarFiles.headOption
        } else {
          logger.warn("node " + node + " was not loaded. parent: " + parentNode)
          None
        }
      }
      maybeJarFile.map{ jarFile =>
        val hash = Hash.calculate(jarFile) 
        val modConfs = depDescriptor.getModuleConfigurations
        val configuration = modConfs.map{ modConf =>
          val depConfs = depDescriptor.getDependencyConfigurations(modConf)
          confString(modConf, depConfs)
        }.mkString(";")
        Dependency(coords, hash, configuration)
      }
    }.toSet
    Seq(Module(coords, artifacts, configurations, attributes, deps))
  }
  
  def add(coords: Coordinates, ivy: Ivy, adept: Adept): Seq[Module] = {
    logger.trace("building dependency tree from ivy...")
    val modules = adeptModule(coords, ivy)
    val all = modules ++ modules.flatMap{ module =>
      module.dependencies.flatMap{ dep => adeptModule(dep.coords, ivy) }
    }
    all.foreach(adept.add)
    all
  }
}
