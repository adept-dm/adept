package adept.download

import akka.actor._

private[adept] class ProgressIndicator extends Actor {
  var bytesDownloaded = 0
  var totalBytes = 0
  val out = System.err
  var lastPrint = ""
  var failed = 0
  var started = 0
  var finished = 0
  var startTime: Long = -1
  
  def printProgress()  = {
    out.print("\b"*lastPrint.length)
    val mbPerSec = (bytesDownloaded.toFloat * 1000)/(1024 * (System.currentTimeMillis - startTime).toFloat )
    val completed = if (totalBytes == 0) 0f else bytesDownloaded.toFloat*100/totalBytes
    val msg = finished+"/"+started + " - %1.0f%% - %1.2f kb/s - total: %1.2f mb" format (completed, mbPerSec, totalBytes.toFloat/(1024f*1024f))
    out.print(msg)
    lastPrint = msg
  }

  def receive = {
    case Started => {
      started += 1
      if (startTime < 0) startTime = System.currentTimeMillis
      printProgress()
    }
    case Initialized(bytes) => {
      totalBytes += bytes
      printProgress()
    }
    case Failed => {
      failed += 1
      printProgress()
      if (started == (failed + finished)) {
        self ! PoisonPill
        out.println
      }
    }
    case Finished => {
      finished += 1
      printProgress()
      if (started == (failed + finished)) {
        self ! PoisonPill
        out.println
      }
    }
    case Update(bytes) => {
      bytesDownloaded += bytes
      printProgress()
    }
  }
}

trait ProgressMsgs
case object Started extends ProgressMsgs
case class Initialized(totalBytes: Int) extends ProgressMsgs
case object Failed extends ProgressMsgs
case object Finished extends ProgressMsgs
case class Update(bytes: Int) extends ProgressMsgs