package adept.core.operations

import adept.core._
import adept.core.models.{Commit => CommitModel, _}
import collection.parallel.ParSeq
import util._
import com.typesafe.scalalogging.slf4j.Logging

private[core] object Commit extends Logging {
  private def verifyModules(modules: ParSeq[(Module, Option[String], Boolean)]): Try[Unit] = {
    logger.warn("did not check if dependencies are correct!")
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
    logger.trace("commit")
    def createCommit(mainSession: Session, moduleHashes: Hash, allStaged: ParSeq[ModuleRowType]) = {
      logger.trace("creating commit...")
      val commit = onlyOption(lastCommit)(mainSession).getOrElse{
        CommitModel(Hash(""), 0, None)
      }
      val commitHash = Hash.calculate(Seq(moduleHashes, commit.hash))
      logger.trace(s"calculated hash: $commitHash")
      val newRows = allStaged.par.map { case (module, _, deleted) =>
        Modules.toRow(module, Some(commitHash), deleted)
      }
      logger.trace(s"inserting ${newRows.length} new rows...")
      Modules.insertAll(newRows.seq: _*)(mainSession)
      Commits.insert(CommitModel(commitHash, (commit.version + 1), None))(mainSession)
      commitHash
    }
    
    stagedDB.withTransaction{ stagedSession: Session =>
      val allModulesQ = Query(Modules) //TODO: paginate
    
      val allStaged = allModulesQ.list()(stagedSession).par.map(Modules.fromRow)
      
      if (allStaged.length > 0) {
        verifyModules(allStaged).flatMap{_ =>
          logger.trace(s"found ${allStaged.length} staged modules...")
          val reCalculatedHashes = allStaged.par.map{ case (module, _, deleted) =>
            Hash.calculate(Seq(module.hash, Hash(if(deleted) "0" else "1") ))  
          }
          val moduleHashes = Hash.calculate(reCalculatedHashes.seq)
          
          val commitHash = mainDB.withTransaction{ mainSession: Session =>
            createCommit(mainSession, moduleHashes, allStaged)
          }
          logger.trace("deleting all modules in staged db...")
          allModulesQ.delete(stagedSession)
          Success(commitHash)
        }
      } else {
        Failure(new Exception("nothing to commit"))
      }
    }
  }
}