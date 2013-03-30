package adept.core.reads

import com.typesafe.scalalogging.slf4j.Logging
import adept.core.models._
import scala.util._

private[core] object GetModule extends Logging {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  import adept.core.db.Types._
  import adept.core.operations.Common._
  import adept.core.operations.Queries._
  
  def apply(hash: Hash, mainDB: Database): Option[Module] ={
    mainDB.withSession{ implicit session: Session =>
      onlyOption(committedModulesForHashQ(hash)).map{ moduleRow =>
        val (module, _, _) = Modules.fromRow(moduleRow)
        module
      }
    }
  }
}