package adept.ivy


import adept.core.models._
import java.io.{File => jFile}
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

object IvyHelpers extends Logging{
  
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

  def adeptModule(coords: Coordinates, ivy: Ivy): Module = {
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
    
    val configurations = moduleDescriptor.getConfigurations().toSet.map{ cachedConf:IvyConfiguration =>
      val c = parent(report).getConfiguration(cachedConf.getName)
      Configuration(
          name = c.getName(),
          description = Option(c.getDescription()),
          extendsFrom = c.getExtends().toSet,
          visibility = c.getVisibility match { 
            case c if c == IvyConfiguration.Visibility.PUBLIC=> Visibility.Public
            case c if c == IvyConfiguration.Visibility.PRIVATE => Visibility.Private
            case somethingElse => throw new Exception("Got unexpected visibility: " + somethingElse)
          },
          deprecated = Option(c.getDeprecated())
      )
    }
    
    val attributes: Map[String, Seq[String]] = Map.empty ++ 
        Option(moduleDescriptor.getDescription()).filter(_.nonEmpty).map(a => "description"->Seq(a)).toMap ++
        Option(moduleDescriptor.getHomePage()).map(a => "home-page"->Seq(a)).toMap
    
    val parentNode = parent(report)
    val artifacts = {
      val artifacts = parentNode.getAllArtifacts.toList.flatMap{ case a: IvyArtifact =>
        val artifactReports = ivy.resolve(parentNode.getId(), resolveOptions(a.getConfigurations().toList: _*), changing).getAllArtifactsReports()
        val thisModuleArtifactReports = artifactReports.filter(_.getArtifact().getId() == a.getId())
        thisModuleArtifactReports.map{ r =>
          val artifact = r.getArtifact()
          val location = r.getArtifactOrigin().getLocation()
          val file = r.getLocalFile()
          val artifactType = artifact.getType() 
          (file, location, artifactType) -> artifact.getConfigurations().toList
        }
      }.toSet
      artifacts.groupBy(_._1).flatMap{ case ((file, location, artifactType), all) => 
        if (file != null && file.exists) {
          val confs =  all.flatMap{case (_, c) => c }
          Set(Artifact.fromFile(file, artifactType, confs, Set(location)))
        } else Set.empty[Artifact]
      }
    }.toSet
    
    val allArtifactReports = report.getAllArtifactsReports.toList
    val ivyArtifacts = allArtifactReports.map( r => r.getArtifact() -> (r.getLocalFile(), resolveEngine.locate(r.getArtifact()).getLocation())).toMap
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
        val exclusionRules = depDescriptor.getAllExcludeRules().map{ ivyExcludeRule =>
          val org = ivyExcludeRule.getId().getModuleId().getOrganisation()
          val name = ivyExcludeRule.getId().getModuleId().getName()
          DependencyExclusionRule(org, name)
        }.toSet
        if (depDescriptor.getAllIncludeRules().nonEmpty) throw new Exception("Include rules are not implemented") //TODO: <- fix this
        
        val configuration = modConfs.map{ modConf =>
          val depConfs = depDescriptor.getDependencyConfigurations(modConf)
          confString(modConf, depConfs)
        }.mkString(";")
        Dependency(coords, hash, configuration, isTransitive = depDescriptor.isTransitive(), exclusionRules = exclusionRules)
      }
    }.toSet
    Module(coords, artifacts, configurations, attributes, deps)
  }
  
  def add(coords: Coordinates, ivy: Ivy, adept: Adept): Set[Module] = {
    logger.trace("building dependency tree from ivy...")
    val module = adeptModule(coords, ivy)
    val children = module.dependencies.flatMap{ dep =>
      add(dep.coordinates, ivy, adept) 
    }
    
    val all = Set(module) ++ children
    
    all.foreach( adept.add )
    //println("******" + modules.map(_.artifacts).mkString("\n\n") + "******")
    all
  }
}
