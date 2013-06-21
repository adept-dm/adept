package adept.utils

import org.eclipse.jgit.transport._

object GitHelpers {

  def sshFactory = {
    new JschConfigSessionFactory {
      override def configure(hc: OpenSshConfig.Host, session: com.jcraft.jsch.Session) {
      }
    }

  }

  def interactiveSshFactory = {
    new JschConfigSessionFactory {
      override def configure(hc: OpenSshConfig.Host, session: com.jcraft.jsch.Session) {
        val provider = new CredentialsProvider() {
          override def isInteractive = true
          override def supports(items: CredentialItem*) = true
          override def get(uri: URIish, items: CredentialItem*) ={
            for (
              item <- items
            ) {
              print(item.getPromptText + ": ")
              val pass = new String(System.console.readPassword())
              item.asInstanceOf[CredentialItem.StringType].setValue(pass)
            }

            true
          }
        }

        val userInfo = new CredentialsProviderUserInfo(session, provider)
        session.setUserInfo(userInfo)

      }
    }
  }

}
