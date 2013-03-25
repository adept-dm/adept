package adept.core.operations

import adept.core.models._
import adept.core.db.DAO.driver.simple._
import util._
import adept.core.db._
import com.typesafe.scalalogging.slf4j.Logging

private[core] object Set extends Logging {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  import adept.core.db.Types._
  import adept.core.operations.Common._
  import adept.core.operations.Queries._
  
  def apply(module: Module, stagedDB: Database, mainDB: Database): Try[Hash] = {
    logger.trace(s"set: $module")
    
    val moduleRow = Modules.toRow(module, commitHash = None, deleted = false)
    stagedDB.withTransaction{ stagedSession: Session =>
      val foundStagedHash = findHash(module.hash, stagedSession)
      
      if (foundStagedHash) {
        logger.trace(s"found staged hash: $foundStagedHash")
        moduleQ(module.hash).update(moduleRow)(stagedSession)
      } else {
        logger.trace(s"no staged hash found")
        mainDB.withTransaction{ mainSession: Session =>
          onlyOption(lastCommittedVersion(module.hash))(mainSession).map{ foundRow =>
            val (foundModule, _, deleted) = Modules.fromRow(foundRow)
            logger.trace(s"found existing module $module (different: ${foundModule != module}, deleted: $deleted)")
            if (deleted || foundModule != module) {
              logger.trace(s"inserting $module...")
              Modules.insert(moduleRow)(stagedSession)
            } else {
              logger.trace(s"skipping $module...")
            }
          }.getOrElse{ //nothing was found 
            logger.trace(s"inserting first module with hash ${module.hash}")
            Modules.insert(moduleRow)(stagedSession)
          }
        }
      }
    }
    Success(module.hash)
  }
}