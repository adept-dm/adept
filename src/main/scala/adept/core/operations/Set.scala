package adept.core.operations

import adept.core.models._
import adept.core.db.DAO.driver.simple._
import util._
import adept.core.db._

private[core] object Set {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  import adept.core.db.Types._
  import adept.core.operations.Common._
  import adept.core.operations.Queries._
  
  def apply(module: Module, stagedDB: Database, mainDB: Database): Try[Hash] = {
    val moduleRow = Modules.toRow(module, commitHash = None, deleted = false)
    stagedDB.withTransaction{ stagedSession: Session =>
      val foundStagedHash = findHash(module.hash, stagedSession)
      
      if (foundStagedHash) {
        moduleQ(module.hash).update(moduleRow)(stagedSession)
      } else {
        mainDB.withTransaction{ mainSession: Session =>
          onlyOption(lastCommitedVersion(module.hash))(mainSession).map{ foundRow =>
            val (foundModule, _, deleted) = Modules.fromRow(foundRow)
            if (deleted || foundModule != module) {
              Modules.insert(moduleRow)(stagedSession)
            } 
          }.getOrElse{ //nothing was found 
            Modules.insert(moduleRow)(stagedSession)
          }
        }
      }
    }
    Success(module.hash)
  }
}