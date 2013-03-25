package adept.core.operations

import adept.core._
import adept.core.models._
import collection.parallel.ParSeq
import util._
import com.typesafe.scalalogging.slf4j.Logging

private[core] object Delete extends Logging {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  import adept.core.db.Types._
  import adept.core.operations.Common._
  import adept.core.operations.Queries._
  
  def apply(hash: Hash, stagedDB: Database, mainDB: Database): Try[Hash] = {
    logger.trace(s"delete: $hash")
    stagedDB.withTransaction{ stagedSession: Session =>
      val foundStagedHash = findHash(hash, stagedSession)
      if (foundStagedHash){
        logger.trace(s"deleted staged module with $hash")
        moduleQ(hash).map(m=> m.commitHash ~ m.deleted).update(None -> true)(stagedSession)
        Success(hash)
      } else {
        mainDB.withTransaction{ mainSession: Session =>
          val foundCommittedHash = findHash(hash, mainSession)
         
          if (foundCommittedHash) {
            logger.trace(s"found committed hash $hash...")
            val lastModuleRow = onlyOption(lastCommittedVersion(hash))(mainSession)
            lastModuleRow.foreach{ row =>
              val (module, commit, deleted) = Modules.fromRow(row)
              logger.trace(s"last version of module $module was found in $commit. is deleted: $deleted")
              if (!deleted) {
                logger.trace(s"inserting delete since last module $module was not deleted")
                Modules.insert(Modules.toRow(module, None, deleted = true))(stagedSession)
              }
            }
            Success(hash)
          } else {
             Failure(new Exception((s"could not find hash: $hash")))
          }
        } 
      }
    }
  }
}