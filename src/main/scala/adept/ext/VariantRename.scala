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

        if (destRepository.exists) {
          destRepository.getRemoteUri(GitRepository.DefaultRemote).foreach { uri =>
            sourceRepository.add(RepositoryLocationsMetadata(Seq(uri)).write(destRepository.name, sourceRepository))
          }
        } else None

        val orderId = Order.findOrderId(sourceId, sourceRepository, sourceCommit) { predicateHash =>
          metadata.hash == predicateHash
        }.getOrElse(throw new Exception("Could not find order id for " + sourceHash + " while renaming " + sourceId + " in " + sourceRepository.dir.getAbsolutePath + " for " + sourceCommit))

        sourceFiles += redirectMetadata.write(sourceId, sourceRepository)
        sourceFiles += Order.add(sourceId, orderId, redirectMetadata.hash, sourceRepository, sourceCommit)

        val destMetadata = metadata.copy(
          requirements = Seq(Requirement(sourceId, constraints = Set.empty, Set.empty)))
        destFiles += destMetadata.write(destId, destRepository)
        destFiles += redirectMetadata.write(sourceId, destRepository)
        //TODO: add resolution results as well
        destFiles += Order.insertNewFile(sourceId, destOrderId, sourceHash, destRepository)
        destFiles += Order.insertNewFile(destId, destOrderId, destMetadata.hash, destRepository)
    }
    (sourceRepository -> sourceFiles, destRepository -> destFiles)
  }
}