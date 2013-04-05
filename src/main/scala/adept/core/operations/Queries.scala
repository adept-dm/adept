package adept.core.operations

import adept.core.db._
import adept.core.db.DAO.driver.simple._
import adept.core.models._
import adept.core.db.Types.ModulesType

private[core] object Queries {
  
  //TODO: change names here. names could start with the return table
  def moduleQ(hash: Hash) = Query(Modules).filter(_.hash === hash.value)
  
  def commitsWithHashes(commits: Set[Hash]) = {
    for {
      commit <- Commits
      if commit.hash.inSet(commits.map(_.value))
    } yield {
      commit.version
    }
  }    
      
  def maxCommitWithHashes(commits: Set[Hash]) = {
    Query(Commits).filter(_.version === commitsWithHashes(commits).max)
  }
  
  def committedVersionsQ(hash: Hash) = for {
    m <- Modules
    c <- Commits
    if m.hash === hash.value && 
       m.commitHash === c.hash
  } yield {
     c.version
  }
  
  def lastCommittedVersion(hash: Hash) = for {
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
  
  def commitsFromHash(hash: Hash) = for{
    fromCommit <- Commits.filter(_.hash === hash.value)
    c <- Commits 
    if c.version > fromCommit.version
  } yield c
  
  def modulesCommitsFromHash(hash: Hash) = for {
    c <- commitsFromHash(hash) 
    m <- Modules if m.commitHash === c.hash 
  } yield (m, c)
  
  
  def moduleQ(thisModuleQ: Query[Modules.type, ModulesType]) = {
    val latestVersionQ = (for {
      m <- thisModuleQ
      c <- Commits if c.hash === m.commitHash
    } yield {
      c.version
    }).max
    
    val moduleQ = for {
      c <- Commits if c.version === latestVersionQ
      m <- thisModuleQ if m.commitHash === c.hash
    } yield {
      m
    }
    moduleQ
  }
  
  def thisModuleQ(coords: Coordinates, hash: Option[Hash])= {
    val thisModuleQ = Query(Modules).filter{m =>
      !m.deleted &&
      m.org === coords.org &&
      m.name === coords.name &&  
      m.version === coords.version
    }
    hash.map{ hash =>
       thisModuleQ.filter(_.hash === hash.value)
    }.getOrElse{
       thisModuleQ
    }
  }

  def moduleForCoordsQ(coords: Coordinates, hash: Option[Hash]) = {
    moduleQ(thisModuleQ(coords, hash))
  }
  
  def modulesForHashQ(hash: Hash) = {
    Query(Modules).filter{m =>
      !m.deleted &&
      m.hash === hash.value
    }
  }
  
  def committedModulesForHashQ(hash: Hash) = {
    moduleQ(modulesForHashQ(hash))
  }
  
  
  def committedModulesForHashesQ(hashes: Set[Hash]) = {
    moduleQ{
      Query(Modules).filter{m =>
        !m.deleted &&
        m.hash.inSet(hashes.map(_.value))
      }
    }
  }
}