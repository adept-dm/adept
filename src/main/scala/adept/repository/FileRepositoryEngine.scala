package adept.repository

import java.io.File
import adept.core.models._

private[adept] class CorruptFileRepositoryException(val repo: FileRepository, override val id: Id, override val errorMsg: String) extends CorruptRepositoryException(s"Corrupt file repository for $id in git repo: ${repo.repoDir}}. Error message was: $errorMsg", id, errorMsg)

/**
 * A Repository engine that writes fast, but reads slow.
 * Probably not the repository engine that should be used.
 * Therefore this is currently private.
 *  
 * WARNING: Also not thread-safe, @see GitRepositoryEngine instead
 * 
 * Cannot be cached reliably which is the reason it is slow.
 */
private[adept] class FileRepositoryEngine(override val baseDir: File, val repos: Set[FileRepository]) extends RepositoryEngine {

  def getArtifacts(variant: Variant, constraints: Set[Constraint]): Set[(Artifact, Set[ArtifactRef])] = {
    val refs = variant.artifacts.filter { artifact =>
      logic.matches(artifact.attributes, constraints)
    }

    refs.groupBy(_.hash).flatMap {
      case (hash, refs) =>
        val repoCandidates = repos.par.filter(_.hasArtifactDescriptor(hash))
        repoCandidates.map { repo =>
          repo.readArtifactDescriptor(hash) match {
            case Right(artifact) => artifact -> refs
            case Left(errorMsg) => throw new Exception(errorMsg)
          }
        }
    }.toSet
  }

  def get(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    val parSet = repos.par.flatMap { repo => //slow and stupid, but not so slow and stupid that we waste good IO on nothing
      repo.readVariants(id) match {
        case Right(variants) => variants
        case Left(errorMsg) => throw new CorruptFileRepositoryException(repo, id, errorMsg)
      }
    }
    val variants = Set.empty[Variant] ++ parSet //FIXME: <-- ++ ???

    logic.filter(id, variants, constraints)
  }
}
