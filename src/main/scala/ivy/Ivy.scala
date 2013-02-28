package adept.ivy

import adept.core._
import java.io.{File => jFile}
import org.apache.ivy._
import org.apache.ivy.core._
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve._
import org.apache.ivy.core.retrieve.RetrieveOptions
import org.apache.ivy.core.report.ResolveReport
import slick.session.Database

object IvyHelpers {
  def load(path: Option[String] = None): Either[String, Ivy] = {
    val ivy = IvyContext.getContext.getIvy
    path.map{ path =>
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
  }
  
  case class IvyTree(node: IvyNode, children: Seq[IvyTree])
  
  private[ivy] def tree(report: ResolveReport, rootModuleConf: String, moduleRevisionId: ModuleRevisionId): Seq[IvyTree] = {
    import collection.JavaConverters._
    
    val nodes = report.getDependencies().asScala.map(_.asInstanceOf[IvyNode])
    val dependencies: Map[ModuleRevisionId, Seq[IvyNode]] = {
      //TODO: this code was ported from java and is mutable :( 
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
  

  
  def add(repoName: String, coords: Coordinates, ivy: Ivy, conf: String)(implicit db: Database) = {
    val resolveOptions = {
      val resolveOptions = new ResolveOptions()
      resolveOptions.setConfs(Array(conf))
      resolveOptions
    }
    val changing = true
    val module = ModuleRevisionId.newInstance(coords.org, coords.name, coords.version)
    val report = ivy.resolve(module, resolveOptions, changing)

    val artifacts = report.getAllArtifactsReports().toList.map( r => r.getArtifact() -> (r.getLocalFile(), r.getArtifactOrigin().getLocation())).toMap
    
    def addModules(trees: Seq[IvyTree]): Either[String, Seq[Module]] = {
      import EitherUtils._
      reduce(trees.map{ tree =>
        
        val depsRes = addModules(tree.children)
        depsRes.right.map{ deps =>
          reduce(tree.node.getAllArtifacts().toSeq.map{ artifact =>
            val (localFile, location) = artifacts(artifact)
            val ivyId = tree.node.getId
            val coords = Coordinates(ivyId.getOrganisation, ivyId.getName, ivyId.getRevision)
            
            val module = Module(coords, Metadata(Map("scope" -> artifact.getConfigurations().mkString(","))), Hash.calculate(coords, localFile))
            val res = Adept.add(repoName, module, deps)
            res
          })
        }.joinRight
        
      }).right.map( _.flatten )
    }
    addModules(tree(report, conf, module))
  }
}