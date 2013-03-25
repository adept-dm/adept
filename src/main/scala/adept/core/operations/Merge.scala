package adept.core.operations

import adept.core.models._
import util._
import com.typesafe.scalalogging.slf4j.Logging

private[core] object Merge extends Logging {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  
  private def applyChanges(changeSets: Seq[ChangeSet])(implicit session: Session) = {
    logger.trace(s"applying $changeSets")
    var lastCommit: Option[Commit] = None
    changeSets.foreach{ changeSet =>
      logger.trace(s"working on: ${changeSet.commit.version} (last commit: ${lastCommit.map(_.version)})")
      if (lastCommit == None || (lastCommit.map(_.version).getOrElse(-1) < changeSet.commit.version)) {
        logger.trace(s"was last commit $lastCommit?")
        lastCommit = Some(changeSet.commit)
      }
      Commits.insert(changeSet.commit)
      changeSet.changes.foreach{ change => 
        Modules.insert(Modules.toRow(change.module, change.commitHash , change.deleted))
      }
    }
    logger.trace(s"last commit was: $lastCommit")
    lastCommit
  }
  
  def findChanges(fromCommit: Hash, mainDB: Database): Try[Seq[ChangeSet]] = {
    logger.trace(s"finding changes for $fromCommit")
    val modulesCommits = mainDB.withSession{ implicit mainSession: Session =>
      Queries.modulesCommitsFromHash(fromCommit).list //TODO: paginate
    }
    val changes = modulesCommits.par.map{ case (moduleRow, commit) =>
      val (module, commitHash, deleted) = Modules.fromRow(moduleRow)
      commit -> Change(module, commitHash.map(Hash.apply), deleted)
    }
    logger.trace(s"found ${changes.length} changes for $fromCommit")
    val changeSets = changes.par.groupBy{ case (commit: Commit, change) => commit }.map{ 
      case (commit, commitChanges) =>
        val changes = commitChanges.map{ case (_, change: Change) => change }
        ChangeSet(commit, changes.seq)
    }
    Success(changeSets.seq.toSeq)
  }
  
  def fastForward(head: Hash, changeSets: Seq[ChangeSet], mainDB: Database): Try[Hash] = {
    mainDB.withTransaction{ implicit session: Session =>
      val versionBump = changeSets.length
      logger.trace(s"fast forwarding from $head")
      val dirtyCommitsQ = Queries.commitsFromHash(head)
      val newCommits = dirtyCommitsQ.list.map{ commit =>
        Query(Commits).filter(_.hash === commit.hash.value).delete //TODO: this will not perform well probably we should do: dirtyCommitsQ.delete but it fails on runtime
        commit.copy(version = commit.version + versionBump)
      }
      logger.trace(s"re-inserting ${newCommits.length} local commits...")
      Commits.insertAll(newCommits: _*)
      applyChanges(changeSets).map{ c =>
        Success(c.hash)
      }.getOrElse{
        Failure(new Exception("no change sets was applied"))
      }
    }
  }
}