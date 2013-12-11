package adept.repository

import java.io.File
import adept.core.models._
import adept.core.resolution.VariantsLoaderEngine
import adept.core.resolution.VariantsLoaderLogic
import net.sf.ehcache.CacheManager

/**
 * A slow and stupid repository engine that reads each file from disk
 * TODO: Implement Git support so we can implement a properly cached behavior
 *
 * Used only for initial testing...
 */
@deprecated("Will be removed and replaced with CachedRepositoryEngine")
class SlowRepositoryEngine(val baseDir: File, val repos: Set[Repository]) extends VariantsLoaderEngine(VariantsLoaderLogic.default) {

  def get(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    val parSet = repos.par.flatMap { repo => //slow and stupid, but not so slow and stupid that we waste good IO on nothing
      repo.readVariants(id) match {
        case Right(variants) => variants
        case Left(errorMsg) => throw new Exception(errorMsg)
      }
    }
    val variants = Set.empty[Variant] ++ parSet //FIXME: <-- ++ ???

    logic.filter(id, variants, constraints)
  }

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

  val artifactsCacheDir = Repository.artifactsCacheDir(baseDir)

  def cache(file: File): File = {
    val hash = Hash.calculate(file)
    val output = getCachedArtifactFile(hash)
    if (output.isFile() && Hash.calculate(output) == hash) {
      //skip
      output
    } else {
      if (output.isFile() && Hash.calculate(output) != hash){ //TODO: re-calculate output hash only once? Assuming this happens rarely, so it is ok for now?
        //TODO: logger.warn instead
        System.err.println("Overwriting corrupt cached artifact: " + output + " was supposed to have hash: " + hash + " but had " + Hash.calculate(output))
        output.delete()
      }
      if ((output.getParentFile.isDirectory || output.getParentFile.mkdirs()) && file.renameTo(output)) output
      else throw new Exception(s"Could not create cache dir to move '$file' to '$output'")
    }
  }

  def getCachedArtifactFile(hash: Hash): File = {
    val level1Dir = hash.value.slice(0, 4)
    val level2Dir = hash.value.slice(4, 8)
    new File(new File(new File(artifactsCacheDir, level1Dir), level2Dir), hash.value)
  }
}