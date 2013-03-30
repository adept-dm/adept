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

  def apply(repoName: String, host: String, port: Int, timeout: FiniteDuration, mainDB: Database, stagedDB: Database): Try[Hash] = {
    logger.trace(s"pull: $repoName from $host:$port")
    val system = ActorSystem("adept-pull")
    try {
      stagedDB.withTransaction{ implicit session: Session => 
        val maybeHead = {
          val commitHashQ = for {
            checkpoint <- Checkpoints if checkpoint.id === Checkpoints.map(_.id).max
          } yield checkpoint.commitHash
          Common.onlyOption(commitHashQ)
        }
        logger.trace(s"last checkpoint: $maybeHead")
        maybeHead.map{ headString =>
          val head = Hash(headString)
          logger.trace(s"fetching from client...")
          val perhapsChangeSets = Await.result(Client.fetch(head, repoName, host, port)(system), timeout)
          perhapsChangeSets.map{ changeSets =>
            logger.trace(s"starting merge of ${changeSets.length} change sets...")
            val checkpoint = Merge.fastForward(head, changeSets, mainDB)
            logger.trace(s"new checkpoint could be: $checkpoint")
            checkpoint.foreach{ h => Checkpoints.autoInc.insert(h.value) }
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
