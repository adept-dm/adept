package adept.core.remote

import adept.core._
import util._
import java.io.File
import scala.slick.session.Database
import scala.collection.parallel.ParSeq
import com.ning.compress.lzf.util.LZFFileInputStream
import adept.core.models._
import akka.actor._
import adept.core.operations._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import com.typesafe.scalalogging.slf4j.Logging
import java.net.URL

private[core] object Pull extends Logging {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  def apply(repoName: String, url: URL, commitHash: Hash, timeout: FiniteDuration, checkpointFile: File, mainDB: Database, stagedDB: Database): Try[Hash] = {
     mainDB.withSession{ implicit session: Session =>
       Common.onlyOption(Query(Commits).filter(_.hash === commitHash.value)).map{ commit =>
         Success(commit.hash)
       }.getOrElse{
         Pull(repoName, url, timeout, checkpointFile, mainDB, stagedDB)
       }
     }
  }
    
  def apply(repoName: String, url: URL, timeout: FiniteDuration, checkpointFile: File, mainDB: Database, stagedDB: Database): Try[Hash] = {
    logger.trace(s"pull: $repoName from $url")
    
    val system = ActorSystem("adept-pull")
    try {
      stagedDB.withTransaction{ implicit session: Session => 
        val maybeHead = Checkpoint.read(checkpointFile)
        logger.trace(s"last checkpoint: $maybeHead")
        maybeHead.map{ head =>
          logger.trace(s"fetching from client...")
          val perhapsChangeSets = Await.result(Client.fetch(head, repoName, url)(system), timeout)
          perhapsChangeSets.map{ changeSets =>
            logger.trace(s"starting merge of ${changeSets.length} change sets...")
            val checkpoint = Merge.fastForward(head, changeSets, mainDB)
            logger.trace(s"new checkpoint could be: $checkpoint")
            checkpoint.foreach{ h => Checkpoint.write(checkpointFile, h) }
            checkpoint
          }.flatten
        }.getOrElse{
          Failure(new Exception("could not find a head from checkpoints. can only pull after cloning"))
        }
      }
    } finally {
      system.shutdown()
    }
  }
}
