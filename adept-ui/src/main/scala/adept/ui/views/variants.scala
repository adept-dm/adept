package adept.ui.views

import spray.http.HttpResponse
import spray.http.HttpEntity
import spray.http.MediaTypes._
import adept.ui.AdeptUIService
import adept.resolution.models.Id
import adept.resolution.models.Variant
import adept.repository.models.RankId
import adept.repository.models.Commit
import adept.repository.models.RepositoryName
object AdeptFormat {
  def binaryVersionString(variants: Set[Variant]) = {
    val binaryVersions = variants.flatMap { variant =>
      variant.attribute(AdeptUIService.BinaryVersionAttributeName).values
    }

    if (binaryVersions.nonEmpty) AdeptUIService.BinaryVersionAttributeName + " = " + binaryVersions.mkString
    else ""
  }

  def versionString(variants: Set[Variant]) = {
    val versions = variants.flatMap { variant =>
      variant.attribute(AdeptUIService.VersionAttributeName).values
    }
    val versionString =
      if (versions.size != 1) throw new Exception("Found differently versioned variants: " + variants)
      else versions.head
    AdeptUIService.VersionAttributeName + " = " + versionString
  }

  def simpleString(variants: Set[Variant]) = {
    val firstString = binaryVersionString(variants)
    firstString + (if (firstString.nonEmpty) "," else "") + versionString(variants)
  }
}
object variants extends jquery with stop {
  def apply(repository: RepositoryName, commit: Commit, rankVariants: Seq[(RankId, Map[String, Set[Variant]])]): HttpResponse = {
    HttpResponse(
      entity = HttpEntity(`text/html`,
        <html>
          { jquery }
          { stopOnClose }
          <body>
            <h3>{ repository.value } @ { commit.value }</h3>
            <table>
              <tr>
                {
                  for ((rankId, _) <- rankVariants) {
                    <th>{ rankId.value }</th>
                  }
                }
              </tr>
              {
                for {
                  (rankId, configuredVariants) <- rankVariants
                } yield {
                  <tr>
                    {
                      for {
                        (base, variants) <- configuredVariants
                      } yield {
                        <td>{ base + " " + AdeptFormat.simpleString(variants) }</td>
                      }
                    }
                  </tr>
                }
              }
            </table>
          </body>
        </html>.toString()))
  }
}