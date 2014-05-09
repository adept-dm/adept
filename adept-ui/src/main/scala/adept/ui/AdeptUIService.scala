package adept.ui

import akka.actor.Actor
import akka.util.Timeout
import spray.can.Http
import spray.http.HttpRequest
import spray.http.HttpMethods.GET
import spray.http.Uri
import spray.http.HttpResponse
import adept.resolution.models.Id
import java.io.File
import scala.util.matching.Regex
import adept.repository.models.RepositoryName
import adept.repository.GitRepository
import adept.repository.metadata.VariantMetadata
import adept.repository.metadata.RankingMetadata
import adept.resolution.models.Attribute

object AdeptUIService {
  val ConfigurationHash = "configuration-hash"
  val VersionAttributeName = "version"
  val BinaryVersionAttributeName = "binary-version"
  val stopURI = "/stop"

  val configuredRepoBaseURI = "/" + "configured" + "/" + "repos"
  val ConfiguredRepoBaseURIRegEx = (configuredRepoBaseURI + "/" + """(.*)""").r
  def configuredRepo(repository: RepositoryName) = configuredRepoBaseURI + "/" + repository.value

  val configuredVariantsBaseURI = "variants"
  val ConfiguredVariantsBaseURIRegEx = (ConfiguredRepoBaseURIRegEx.pattern + "/" + configuredVariantsBaseURI + "/" + """(.*)""").r
  def configuredVariants(repository: RepositoryName, base: String) = {
    configuredRepoBaseURI + "/" + repository.value + "/" + configuredVariantsBaseURI + "/" + base
  }

}

class AdeptUIService extends Actor {
  import AdeptUIService._
  import scala.concurrent.duration._
  implicit val timeout: Timeout = 1.second

  import context.dispatcher

  val baseDir = new File(System.getProperty("user.home"), ".adept") //TODO: configurable

  def receive = {
    case _: Http.Connected =>
      sender ! Http.Register(self)
    case HttpRequest(GET, Uri.Path("/"), _, _, _) | HttpRequest(GET, Uri.Path(""), _, _, _) =>
      val repoDir = new File(baseDir, "repos")
      val repositories = repoDir.listFiles.flatMap { f =>
        if (f.isDirectory()) Some(RepositoryName(f.getName))
        else None
      }.toSet
      sender ! views.repositories(repositories)
    case HttpRequest(GET, Uri.Path(path), _, _, _) if ConfiguredVariantsBaseURIRegEx.findAllMatchIn(path).nonEmpty =>
      path match {
        case ConfiguredVariantsBaseURIRegEx(repoString, base) =>
          val repository = new GitRepository(baseDir, RepositoryName(repoString))
          val commit = repository.getHead
          val rankedVariants = VariantMetadata.listIds(repository, commit)
            .filter(_.value.startsWith(base))
            .flatMap { id =>
              RankingMetadata.listRankIds(id, repository, commit).map { rankId =>
                rankId -> {
                  val ranking = RankingMetadata.read(id, rankId, repository, commit).getOrElse(throw new Exception("FATAL! Could not read ranking: " + (id, rankId, repository, commit)))
                  ranking.variants.map { hash =>
                    VariantMetadata
                      .read(id, hash, repository, commit, checkHash = true)
                      .getOrElse(throw new Exception("FATAL! Could not read variant: " + (id, hash, repository, commit)))
                      .toVariant(id)
                  }
                }
              }
            }
          val groupedRankedVariants = rankedVariants.groupBy { case (rankId, variants) => rankId }
          val rankedVariantsByConfig = groupedRankedVariants.map {
            case (rankId, rankAndVariants) =>
              val variants = rankAndVariants.flatMap {
                case (_, variants) =>
                  variants
              }
              val variantsByConfigHash = variants.groupBy(_.attribute(ConfigurationHash))
              val variantsByConfig = variantsByConfigHash.map {
                case (configHash, variants) =>
                  val base = variants.map(_.id.value).reduce(_ intersect _)
                  base -> variants.toSet
              }.toMap
              rankId -> variantsByConfig
          }
          val sortedRankedVariantsByConfig = rankedVariantsByConfig.toSeq.sortBy { case (rankId, _) => rankId.value }
          sender ! views.variants(repository.name, commit, sortedRankedVariantsByConfig)
      }
    case HttpRequest(GET, Uri.Path(path), _, _, _) if ConfiguredRepoBaseURIRegEx.findAllMatchIn(path).nonEmpty =>
      println(ConfiguredRepoBaseURIRegEx.pattern)
      path match {
        case ConfiguredRepoBaseURIRegEx(repoString) =>
          val repository = new GitRepository(baseDir, RepositoryName(repoString))
          val commit = repository.getHead
          val byConfig = VariantMetadata.listIds(repository, repository.getHead).flatMap { id =>
            val variants = VariantMetadata.listVariants(id, repository, commit).flatMap { hash =>
              VariantMetadata.read(id, hash, repository, commit, checkHash = true).map(_.toVariant(id))
            }
            variants
          }.groupBy { variant =>
            variant.attribute(ConfigurationHash)
          }
          val ids = byConfig.map {
            case (configAttribute, variants) =>
              variants.map(_.id.value).reduce(_ intersect _)
          }.toSet

          sender ! views.repository(repository.name, commit, ids)
      }
    case HttpRequest(GET, Uri.Path(AdeptUIService.stopURI), _, _, _) =>
      Console.out.println("Shutting down...")
      sender ! HttpResponse(entity = "Shutting down...")
      sender ! Http.Close
      context.system.shutdown()
  }

}
