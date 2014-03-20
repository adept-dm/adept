package adept.ivy

import org.apache.ivy.util.DefaultMessageLogger
import org.apache.ivy.util.Message

class AdeptIvyMessageLogger(level: Int = Message.MSG_ERR) extends DefaultMessageLogger(level) {
  var i = 0

  override def doProgress(): Unit = {
    val indicator = if (i == 0) "-"
    else if (i == 1) "/"
    else if (i == 2) "-"
    else if (i == 3) "\\"
    else if (i == 4) "|"
    else {
      i = 0
      "/"
    }
    i = i + 1
    //System.out.print("\r" * 80 + " " * 80 + "\r" * 80)
    System.out.print(indicator + "\r")
  }

  override def doEndProgress(ivyMsg: String): Unit = {
    //pass
  }
}
