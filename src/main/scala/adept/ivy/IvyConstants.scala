package adept.ivy
import org.apache.ivy.util.Message
import adept.ext.ConfigurationHelpers

object IvyConstants {
  val IvyNameAttribute = "ivy-name"
  val IvyOrgAttribute = "ivy-organisation"

  val ConfigurationHashAttribute = ConfigurationHelpers.ConfigurationHashAttribute
  val ConfigurationAttribute = ConfigurationHelpers.ConfigurationAttribute
  val ArtifactConfAttribute = ConfigurationHelpers.ArtifactConfAttribute
  val IdConfig = ConfigurationHelpers.IdConfig

  lazy val errorIvyLogger = new AdeptIvyMessageLogger(Message.MSG_ERR)
  lazy val warnIvyLogger = new AdeptIvyMessageLogger(Message.MSG_WARN)
  lazy val infoIvyLogger = new AdeptIvyMessageLogger(Message.MSG_INFO)
  lazy val debugIvyLogger = new AdeptIvyMessageLogger(Message.MSG_DEBUG)
}