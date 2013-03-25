package adept.core.db

import adept.core.models._
import adept.core.db.DAO.driver.simple._

object Checkpoints extends Table[(Int, String)]("CHECKPOINTS"){
  def id = column[Int]("CHECKPOINT_ID", O.PrimaryKey, O.AutoInc)
  def commitHash = column[String]("CHECKPOINT_COMMIT_HASH", O.NotNull)
  
  def * = id ~ commitHash
  
  def autoInc = commitHash
}