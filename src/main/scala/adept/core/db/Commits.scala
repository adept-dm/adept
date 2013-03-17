package adept.core.db

import DAO.driver.simple._
import Types._

object Commits extends Table[RepositoryVersionsType]("COMMITS") {
  def hash = column[String]("COMMIT_HASH", O.PrimaryKey)
  def version = column[Int]("COMMIT_VERSION", O.NotNull)
  def * = hash ~ version
}