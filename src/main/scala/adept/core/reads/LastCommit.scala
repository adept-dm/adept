package adept.core.reads

import adept.core.models._
import scala.util._

object LastCommit {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  import adept.core.db.Types._
  import adept.core.operations.Common._
  import adept.core.operations.Queries._
  
  def apply(commits: Set[Hash], mainDB: Database): Option[Hash] = {
    mainDB.withSession{ implicit session: Session =>
      onlyOption(maxCommitWithHashes(commits).map(_.hash)).map(Hash.apply)
    }
  }
}