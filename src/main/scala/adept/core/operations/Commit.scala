package adept.core.operations

import adept.core._
import adept.core.models._
import collection.parallel.ParSeq
import util._

private[core] object Commit extends Logger {
  private def verifyModules(modules: ParSeq[(Module, Option[String], Boolean)]): Try[Unit] = {
    warn("did not check if dependencies are correct!")
    val missing = modules.map{ case (module, commitHash, deleted) =>
      //TODO: fix this
    }
    Success()
  }
  
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  import adept.core.db.Types._
  import adept.core.operations.Common._
  import adept.core.operations.Queries._
  
  def apply(stagedDB: Database, mainDB: Database): Try[Hash] = {
    
    def createCommit(mainSession: Session, moduleHashes: Hash, allStaged: ParSeq[ModuleRowType]) = {
      val (lastHash, lastVersion) = onlyOption(lastCommit)(mainSession).getOrElse{
        "" -> 0
      }
      val commitHash = Hash.calculate(Seq(moduleHashes, Hash(lastHash)))
      val newRows = allStaged.par.map { case (module, _, deleted) =>
        Modules.toRow(module, Some(commitHash), deleted)
      }
      Modules.insertAll(newRows.seq: _*)(mainSession)
      Commits.insert(commitHash.value, (lastVersion + 1))(mainSession)
      commitHash
    }
    
    stagedDB.withTransaction{ stagedSession: Session =>
      val allModulesQ = Query(Modules) //TODO: paginate
    
      val allStaged = allModulesQ.list()(stagedSession).par.map(Modules.fromRow)
      
      if (allStaged.length > 0) {
        verifyModules(allStaged).flatMap{_ =>
          
          val reCalculatedHashes = allStaged.par.map{ case (module, _, deleted) =>
            Hash.calculate(Seq(module.hash, Hash(if(deleted) "0" else "1") ))  
          }
          val moduleHashes = Hash.calculate(reCalculatedHashes.seq)
          
          val commitHash = mainDB.withTransaction{ mainSession: Session =>
            createCommit(mainSession, moduleHashes, allStaged)
          }
          
          allModulesQ.delete(stagedSession)
          Success(commitHash)
        }
      } else {
        Failure(new Exception("nothing to commit"))
      }
    }
  }
}