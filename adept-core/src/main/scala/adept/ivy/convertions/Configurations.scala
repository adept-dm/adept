package adept.ivy.convertions

import adept.core.models._
import org.apache.ivy._
import org.apache.ivy.core._
import org.apache.ivy.core.resolve._
import org.apache.ivy.core.report.ResolveReport
import scala.util._
import java.io.File
import adept.core.Adept
import org.apache.ivy.util.Message
import org.apache.ivy.util.DefaultMessageLogger
import adept.utils.Logging
import collection.JavaConverters._
import org.apache.ivy.core.report.ResolveReport
import org.apache.ivy.core.module.descriptor.{ Artifact => IvyArtifact }
import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }

private[ivy] object Configurations {
  import adept.ivy.utils.IvyHelpers._

  def convert(parent: IvyNode, cachedConf: IvyConfiguration): Configuration = {
    val c = parent.getConfiguration(cachedConf.getName)
    Configuration(
      name = c.getName(),
      description = Option(c.getDescription()),
      extendsFrom = c.getExtends().toSet,
      visibility = c.getVisibility match {
        case c if c == IvyConfiguration.Visibility.PUBLIC => Visibility.Public
        case c if c == IvyConfiguration.Visibility.PRIVATE => Visibility.Private
        case somethingElse => throw new Exception("Got unexpected visibility: " + somethingElse)
      },
      deprecated = Option(c.getDeprecated()))
  }

}