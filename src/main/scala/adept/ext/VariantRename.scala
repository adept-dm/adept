package adept.ext

import java.io.File
import adept.resolution.models.Id
import adept.repository.models.RepositoryName
import adept.repository.models.VariantHash
import adept.repository.models.Commit
import adept.repository.serialization.Order
import adept.repository.GitRepository
import adept.repository.serialization.VariantMetadata
import adept.resolution.models.Attribute
import adept.resolution.models.Requirement
import adept.repository.models.RepositoryLocations
import adept.repository.serialization.RepositoryLocationsMetadata
import adept.repository.serialization.ResolutionResultsMetadata

object VariantRename {
  def rename(baseDir: File, sourceId: Id, sourceName: RepositoryName, sourceCommit: Commit, destId: Id, destName: RepositoryName): ((GitRepository, Set[File]), (GitRepository, Set[File])) = { //source repo and files, dest repo and files
    val sourceRepository = new GitRepository(baseDir, sourceName)
    val destRepository = new GitRepository(baseDir, destName)

    var sourceFiles = Set.empty[File]
    var destFiles = Set.empty[File]

    var activeSourceVariants = Order.activeVariants(sourceId, sourceRepository, sourceCommit)

    val destOrderStart = if (!destRepository.exists) {
      destRepository.init()
      0
    } else {
      val activeDestVariants = Order.activeVariants(sourceId, destRepository, destRepository.getHead)
      activeDestVariants.foreach { hash =>
        activeSourceVariants -= hash //remove any variants we have already added
      }
      activeDestVariants.size
    }
    val newDestOrders = {
      if (activeSourceVariants.isEmpty) Seq.empty
      else {
        Order.getXOrderId(destRepository, destOrderStart, activeSourceVariants.size).toSeq.sortBy(_.value)
      }
    }

    val zippedOrdersVariants = newDestOrders zip activeSourceVariants.toSeq.sortBy(_.value)
    zippedOrdersVariants.foreach {
      case (destOrderId, sourceHash) =>
        val metadata = VariantMetadata.read(sourceId, sourceHash, sourceRepository, sourceCommit).getOrElse(throw new Exception("Cannot rename because we expected: " + sourceHash + " for " + sourceId + " in " + sourceRepository.dir.getAbsolutePath + " but we could not find it"))

        val redirectMetadata = VariantMetadata(
          attributes = Seq(Attribute("redirect", Set(destName.value + "/" + destId))),
          artifacts = Seq.empty,
          requirements = Seq(Requirement(destId, constraints = Set.empty, Set.empty)))

        val orderId = Order.findOrderId(sourceId, sourceRepository, sourceCommit) { predicateHash =>
          metadata.hash == predicateHash
        }.getOrElse(throw new Exception("Could not find order id for " + sourceHash + " while renaming " + sourceId + " in " + sourceRepository.dir.getAbsolutePath + " for " + sourceCommit))

        //source files, for each order id:
        //  write the new variant with redirect info
        sourceFiles += redirectMetadata.write(sourceId, sourceRepository)
        //  add the hash of the variant we just add to order file
        sourceFiles += Order.add(sourceId, orderId, redirectMetadata.hash, sourceRepository, sourceCommit)
        //  write repository locations for the new repository if it exists
        if (destRepository.exists) {
          destRepository.getRemoteUri(GitRepository.DefaultRemote).foreach { uri =>
            sourceRepository.add(RepositoryLocationsMetadata(Seq(uri)).write(destRepository.name, sourceRepository))
          }
        } else None

        val destMetadata = metadata.copy(
          requirements = Seq(Requirement(sourceId, constraints = Set.empty, Set.empty)))
        //dest files, for each order id:
        //   write the new destination variant (with the extra attribute)
        destFiles += destMetadata.write(destId, destRepository)
        //   if we do not have this hash in the redirect info already:
        //   write the same redirect variant in this repository as well
        destFiles += redirectMetadata.write(sourceId, destRepository)
        destFiles ++= Order.insertNewFile(sourceId, redirectMetadata.hash, destRepository, destRepository.getHead)
        destFiles += Order.insertNewFile(destId, destOrderId, destMetadata.hash, destRepository)

      //        destFiles ++= ResolutionResultsMetadata.read(sourceId, metadata.hash, sourceRepository, sourceCommit).map { resolutionResults =>
      //          resolutionResults.write(destId, destMetadata.hash, destRepository)
      //          resolutionResults.values.map{ resolutionResult =>
      //            resolutionResult.repository //TODO: repository locations
      //          }
      //        }
    }
    (sourceRepository -> sourceFiles, destRepository -> destFiles)
  }
}