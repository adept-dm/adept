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
import adept.resolution.models.Attribute
import adept.repository.models.VariantHash

object repository extends jquery with stop {
  def apply(repository: RepositoryName, commit: Commit, baseStrings: Set[String]): HttpResponse = {
    HttpResponse(
      entity = HttpEntity(`text/html`,
        <html>
          { jquery }
          { stopOnClose }
          <body>
            <h3>{ repository.value }</h3>
            <ul>
              {
                for (base <- baseStrings.toSeq.sorted) yield {
                  <li><a href={ AdeptUIService.configuredVariants(repository, base) }>{ base }</a></li>
                }
              }
            </ul>
          </body>
        </html>.toString()))
  }
}