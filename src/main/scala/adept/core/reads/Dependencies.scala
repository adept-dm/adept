package adept.core.reads

import adept.core.models._
import scala.util._
import adept.core.operations.Common
import com.typesafe.scalalogging.slf4j.Logging
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

private[core] object Dependencies extends Logging {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  import adept.core.db.Types._
  import adept.core.operations.Common._
  import adept.core.operations.Queries._
  
  def apply(coords: Coordinates, hash: Option[Hash], mainDB: Database, stagedDB: Database): Try[Set[(Module, Hash)]] ={
    import scala.concurrent.ExecutionContext.Implicits.global //TODO: create a separate execution context
    //query staged and main at the same time: TODO: some other parallel way to do this is better?
    val possibleMainModules = Future{ getModules(coords, hash, moduleForCoordsQ(coords, hash), committedModulesForHashQ _, mainDB) } 
    val possibleStagedModules = Future{ getModules(coords, hash, thisModuleQ(coords, hash), modulesForHashQ _, stagedDB) }
    val mainModules = Await.result(possibleMainModules, Duration(1024, TimeUnit.HOURS)) //TODO: this timeout is not very nice
    val stagedModule= Await.result(possibleStagedModules, Duration(1024, TimeUnit.HOURS))
    if (stagedModule.size > 0) {
      Failure(new Exception("Found staged modules related to this module: " + stagedModule.mkString("\n") + ". Please remove them or commit or reset the changes."))
    } else {
      Success(mainModules.map{ 
         case (module, Some(commit)) => module -> commit
         case (module, _) => throw new Exception(s"could not find commit which was expected for: $module")
      })
    }
  }
  
  private def getModules(coords: Coordinates, hash: Option[Hash], moduleQ: Query[Modules.type, ModulesType], depModuleQ: Hash => Query[Modules.type, ModulesType], db: Database): Set[(Module, Option[Hash])] ={
    db.withSession{ implicit session: Session =>
      logger.trace(s"check staged: $coords!$hash")
      Common.onlyOption(moduleQ).map{ moduleRow =>
        val (module, maybeCommit, deleted) = Modules.fromRow(moduleRow)
        assert(!deleted, s"got a deleted module though the query should exclude this: $moduleRow")
        Set(module -> maybeCommit.map(Hash.apply)) ++ getDeps(module, depModuleQ) 
      }.getOrElse{
        Set.empty
      }
    }
  }
  
  private def getDeps(module: Module, modulesQ: Hash => Query[Modules.type, ModulesType])(implicit session: Session): Set[(Module, Option[Hash])] = {
    logger.trace(s"deps for: $module")
    module.deps.flatMap{ hash =>
      Common.onlyOption(modulesQ(hash)).toList.flatMap{ moduleRow => //TODO: this is not very performant, should do one query for all
        val (module, maybeCommit, deleted) = Modules.fromRow(moduleRow)
        Set(module -> maybeCommit.map(Hash.apply)) ++ getDeps(module, modulesQ)
      }
    }
  }
}