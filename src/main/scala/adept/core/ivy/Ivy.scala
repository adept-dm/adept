package adept.core.ivy

import adept.core._
import java.io.{File => jFile}
import org.apache.ivy._
import org.apache.ivy.core._
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve._
import org.apache.ivy.core.retrieve.RetrieveOptions
import org.apache.ivy.core.report.ResolveReport
import slick.session.Database
import scala.util._
import java.io.File
import com.typesafe.scalalogging.slf4j.Logging

object IvyHelpers extends Logging {
  def load(path: Option[String] = None, ivyRoot: Option[File] = None): Either[String, Ivy] = { //TODO make this a Try instead
    val ivy = IvyContext.getContext.getIvy
    val res = path.map{ path =>
      val ivySettings = new jFile(path)
      if (!ivySettings.isFile) {
        Left(s"$ivySettings is not a file")
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
      ivyRoot.foreach(settings.setDefaultIvyUserDir) //FIXME: TODO this does not WORK?!?!
      ivy.setSettings(settings)
      ivy
    }
  }
  
  case class IvyTree(node: IvyNode, children: Seq[IvyTree])
  
  private[ivy] def tree(report: ResolveReport, rootModuleConf: String, moduleRevisionId: ModuleRevisionId): Seq[IvyTree] = {
    import collection.JavaConverters._
    
    val nodes = report.getDependencies().asScala.map(_.asInstanceOf[IvyNode]).filter(!_.isEvicted(rootModuleConf))
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
  

  import adept.core.models._
  import adept.core.TryHelpers._
    
  def set(coords: Coordinates, ivy: Ivy, conf: String, adept: Adept): Try[Seq[Module]] = {
    val resolveOptions = {
      val resolveOptions = new ResolveOptions()
      resolveOptions.setConfs(Array(conf))
      resolveOptions.setCheckIfChanged(true)
      resolveOptions.setRefresh(true)
      resolveOptions.setDownload(true)
      resolveOptions
    }
    val changing = true
    val module = ModuleRevisionId.newInstance(coords.org, coords.name, coords.version)
    val report = ivy.resolve(module, resolveOptions, changing)
    val resolveEngine = ivy.getResolveEngine()
    val artifacts = report.getAllArtifactsReports().toList.map( r => r.getArtifact() -> (r.getLocalFile(), resolveEngine.locate(r.getArtifact()).getLocation())).toMap
        
    def addModules(trees: Seq[IvyTree]): Try[Seq[Module]] = {
      reduce(trees.map{ tree =>
        val depsRes = addModules(tree.children)
        depsRes.map{ deps =>
          tree.node.getAllArtifacts().toSeq.map{ artifact =>
            val (localFile, location) = artifacts(artifact)
            val ivyId = tree.node.getId
            val coords = Coordinates(ivyId.getOrganisation, ivyId.getName, ivyId.getRevision)
            val metadata = Metadata(Map("ivy-configurations" -> artifact.getConfigurations().mkString(","))) //TODO: add resolver to metadata?
            val module = Module.fromFile(localFile, coords, metadata, Set(Artifact(location)), deps.map(_.hash).toSet)
            val res = adept.set(module)
            module
          }
        }
      }).map(_.flatten)
    }
    logger.trace("building dependency tree from ivy...")
    addModules(tree(report, conf, module))
  }
}