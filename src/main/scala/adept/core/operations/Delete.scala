package adept.core.operations

import adept.core._
import adept.core.models._
import collection.parallel.ParSeq
import util._

private[core] object Delete {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  import adept.core.db.Types._
  import adept.core.operations.Common._
  import adept.core.operations.Queries._
  
  def apply(hash: Hash, stagedDB: Database, mainDB: Database): Try[Hash] = {
    stagedDB.withTransaction{ stagedSession: Session =>
      val foundStagedHash = findHash(hash, stagedSession)
      if (foundStagedHash){
        moduleQ(hash).map(m=> m.commitHash ~ m.deleted).update(None -> true)(stagedSession)
        Success(hash)
      } else {
        mainDB.withTransaction{ mainSession: Session =>
          val foundCommittedHash = findHash(hash, mainSession)
         
          if (foundCommittedHash) {
            val lastModuleRow = onlyOption(lastCommitedVersion(hash))(mainSession)
            lastModuleRow.foreach{ row =>
              val (module, commit, deleted) = Modules.fromRow(row)
              if (!deleted) {
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