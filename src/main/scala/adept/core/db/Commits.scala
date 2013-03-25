package adept.core.db

import DAO.driver.simple._
import Types._
import java.sql.Timestamp
import adept.core.models._

object Commits extends Table[Commit]("COMMITS") {
  def hash = column[String]("COMMIT_HASH", O.PrimaryKey)
  def version = column[Int]("COMMIT_VERSION", O.NotNull)
  def pushed = column[Option[Timestamp]]("COMMIT_PUSHED")
  def * = hash ~ version ~ pushed <> ({
    (hashString, version, pushed) => Commit(Hash(hashString), version, pushed)
  }, {
    c => Some((c.hash.value, c.version, c.pushed))
  })
  
  def versionIdx= index("COMMIT_VERSION_INDEX", (version), unique = true)
}