package adept.ui.views

import spray.http.HttpResponse
import spray.http.HttpEntity
import spray.http.MediaTypes._
import adept.ui.AdeptUIService
import adept.resolution.models.Id
import adept.repository.models.RepositoryName

object repositories extends jquery with stop {
  def apply(repositories: Set[RepositoryName]): HttpResponse = {
    HttpResponse(
      entity = HttpEntity(`text/html`,
        <html>
          { jquery }
          { stopOnClose }
          <body>
            <ul>
            {
              for (repo <- repositories) yield {
                <li><a href={AdeptUIService.configuredRepo(repo)}>{repo.value}</a></li>
              }
            }
            </ul>
          </body>
        </html>.toString()))
  }
}