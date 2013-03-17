package adept.core.operations

import adept.core.db.DAO.driver.simple._
import adept.core.models._

object Common {
  def onlyOption[A, B](q: Query[A, B])(implicit session: Session) = {
    val all = q.list
    if (all.length > 1) throw new Exception(s"FATAL: expected to find maximum one, but found: $all")
    all.headOption
  }
  
  def findHash(hash: Hash, session : Session) = {
    onlyOption(Query(Queries.moduleQ(hash).exists))(session).getOrElse(false)
  }
}