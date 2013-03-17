package adept.core.operations

import adept.core.db._
import adept.core.db.DAO.driver.simple._
import adept.core.models._

private[core] object Queries {
  def moduleQ(hash: Hash) = Query(Modules).filter(_.hash === hash.value)
  
  def committedVersionsQ(hash: Hash) = for {
    m <- Modules
    c <- Commits
    if m.hash === hash.value && 
       m.commitHash === c.hash
  } yield {
     c.version
  }
  
  def lastCommitedVersion(hash: Hash) = for {
    c <- Commits if c.version === committedVersionsQ(hash).max
    m <- Modules if m.hash === hash.value && m.commitHash === c.hash
  } yield {
    m
  }
  
  def lastCommit = for {
    c <- Commits if c.version === Query(Commits).map(_.version).max
  } yield {
    c
  }
}