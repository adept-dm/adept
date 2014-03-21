package adept.ext

import java.io.File
import adept.resolution.models.Id
import adept.repository.models.RepositoryName
import adept.repository.models.VariantHash
import adept.repository.models.Commit
import adept.repository.serialization.Order
import adept.repository.GitRepository

object VariantRename {
  def rename(baseDir: File, sourceId: Id, sourceName: RepositoryName, sourceCommit: Commit, destId: Id, destName: RepositoryName) = {
    val sourceRepository = new GitRepository(baseDir, sourceName)
    val destRepository = new GitRepository(baseDir, destName)
    Order.listActiveOrderIds(sourceId, sourceRepository, sourceCommit).foreach { orderId =>
      //add new variant for each Active Order Id
      //  that depends on the dest Id and nothing else 
      //  and has an attribute "redirect-hash"
      //  add repository locations pointing to the new one if it exists (read the git repo and extract repository location)
      //write variant metadata in the dest id/repo, (create repository and a new order id if needed)
      //  add a requirement back to the source Id and constrain it to "redirect-hash"
      //  also copy repository and artifact locations needed from source and  
      
      //Goal: you can use old variants as before, but you can upgrade and it will use the new one. 
      //It will fail if you use both an old variant id if you have any requirements and a new "redirected". if you do not have any requirements, then 
      //You can safely upgrade, you can check that the latest variant in the repo you where using are redirectng to another
      //Next upfix: lockfile.upgrade()
    }

  }
}