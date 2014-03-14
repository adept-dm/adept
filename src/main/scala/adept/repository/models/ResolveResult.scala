package adept.repository.models

import adept.repository.models._
import adept.resolution.models._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import adept.utils.OrderingHelpers

/** The resolve result for each Id: answers the who we found (the variant hash) and where we found it (commit and repository) */
case class ResolveResult(id: Id, repository: RepositoryName, commit: Commit, variant: VariantHash)
