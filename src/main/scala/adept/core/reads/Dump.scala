package adept.core.reads

import com.typesafe.scalalogging.slf4j.Logging
import adept.core.models._

private[core] object Dump extends Logging {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  import adept.core.db.Types._
  import adept.core.operations.Common._
  import adept.core.operations.Queries._

  def apply(db: Database): List[(Module, Option[Hash], Boolean)] ={
    logger.trace(s"dumping database...")
    db.withSession{ implicit session: Session =>
      Query(Modules).list.map{ row =>
        val (module, hashString, deleted) = Modules.fromRow(row)
        (module, hashString.map(Hash.apply), deleted)
      }
    }
  
  }

}