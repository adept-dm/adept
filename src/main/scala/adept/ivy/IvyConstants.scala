package adept.ivy
import org.apache.ivy.util.Message

object IvyConstants {
  val IvyNameAttribute = "ivy-name"
  val IvyOrgAttribute = "ivy-organisation"

  val ConfigurationHashAttribute = "configuration-hash" //TODO: delete this
  val ConfigurationAttribute = "configuration"
  val ArtifactConfAttribute = "configurations"
  val IdConfig = "config"

  lazy val errorIvyLogger = new AdeptIvyMessageLogger(Message.MSG_ERR)
  lazy val warnIvyLogger = new AdeptIvyMessageLogger(Message.MSG_WARN)
  lazy val infoIvyLogger = new AdeptIvyMessageLogger(Message.MSG_INFO)
  lazy val debugIvyLogger = new AdeptIvyMessageLogger(Message.MSG_DEBUG)
}