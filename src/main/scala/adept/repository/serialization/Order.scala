package adept.repository.serialization

import adept.repository.models.VariantHash
import adept.repository.Repository
import adept.resolution.models.Id
import java.io.File
import java.io.FileWriter
import java.io.FileInputStream
import java.io.FileOutputStream
import adept.repository.GitRepository
import adept.repository.models.Commit
import java.io.BufferedReader
import java.io.Reader
import java.io.InputStreamReader
import java.io.FileReader
import java.io.BufferedWriter
import adept.repository.models.OrderId
import java.io.BufferedInputStream
import adept.utils.Hasher

case class IllegalOrderStateException(repository: Repository, reason: String) extends Exception("Order file(s) in " + repository.dir.getAbsolutePath + " are not well formed. Details: " + reason)

/**
 * Used to manage the variant order for a repository.
 *
 * Contains lines of sets of variant hashes.
 * In Adept, only one set of variant will be used for resolution.
 * The first line is considered to be the "latest" sets of variants.
 * Can find the set of variants that can be used together at any given time.
 */
object Order {
  private def assertNewHash(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit) = {
    listActiveOrderIds(id, repository, commit).par.foreach { orderId => //NOTICE .par TODO: use IO execution context
      val currentFile = repository.getOrderFile(id, orderId)
      io.Source.fromFile(currentFile).getLines.foreach { line =>
        if (line == hash.value) {
          throw IllegalOrderStateException(repository, "File: " + currentFile.getAbsolutePath + " already has " + hash + " on line: " + line)
        }
      }
    }
  }

  private def addLine(line: String, getFos: => FileOutputStream) = {
    var fos: FileOutputStream = null
    try {
      fos = getFos
      fos.write((line + '\n').getBytes)
      fos.flush()
    } finally {
      if (fos != null) fos.close()
    }
  }

  private def assertNewOrder(id: Id, orderId: OrderId, repository: GitRepository, commit: Commit) = {
    if (repository.getOrderFile(id, orderId).isFile) throw IllegalOrderStateException(repository, "New order " + orderId.value + " cannot be created because " + repository.getOrderFile(id, orderId).getAbsolutePath + " is a file already.")
    if (listActiveOrderIds(id, repository, commit).contains(orderId)) {
      throw IllegalOrderStateException(repository, "New order " + orderId.value + " cannot be created because " + repository.asGitPath(repository.getOrderFile(id, orderId)) + " already exists?")
    }
  }

  def findOrderId(id: Id, repository: GitRepository, commit: Commit)(predicate: VariantHash => Boolean): Option[OrderId] = {
    val orderIds = listActiveOrderIds(id, repository, commit)
    var foundOrderId: Option[OrderId] = None
    var invalidated = false
    orderIds.foreach { orderId =>
      repository.usingOrderInputStream(id, orderId, commit) {
        case Right(Some(is)) =>
          if (foundOrderId.isEmpty) {
            val allLinesMatch = io.Source.fromInputStream(is).getLines.forall { line =>
              predicate(VariantHash(line.trim()))
            }
            if (allLinesMatch) foundOrderId = Some(orderId)
          } else {
            val oneLineMatch = io.Source.fromInputStream(is).getLines.exists { line =>
              predicate(VariantHash(line.trim()))
            }
            if (oneLineMatch && foundOrderId.isDefined) {
              invalidated = true
            }
          }
        case _ => //pass
      }
    }
    if (!invalidated) foundOrderId
    else None
  }

  def insertNewFile(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit): Set[File] = {
    assertNewHash(id, hash, repository, commit)
    val currentIdsSize = listActiveOrderIds(id, repository, commit).size
    val orderId = getXOrderId(id, repository, start = currentIdsSize, N = 1)
      .headOption.getOrElse(throw new Exception("Could not create a new order id for: " + id + " in " + repository.dir.getAbsolutePath + " for " + commit))
    assertNewOrder(id, orderId, repository, commit)
    val newOrderFile = repository.getOrderFile(id, orderId)
    try {
      val append = true
      addLine(hash.value, new FileOutputStream(newOrderFile, append))
      Set(newOrderFile)
    } catch {
      case e: Exception =>
        newOrderFile.delete()
        throw IllegalOrderStateException(repository, "An exception was thrown: " + e.getCause() + " (files where deleted)")
    }
  }

  def add(id: Id, orderId: OrderId, hash: VariantHash, repository: GitRepository, commit: Commit): File = {
    repository.usingOrderInputStream(id, orderId, commit) { //Sigh, I think we should have another look at this messy code
      case Right(Some(is)) =>
        val tmpFile = File.createTempFile("adept-order-tmp-add-", repository.name.value)
        val orderFile = repository.getOrderFile(id, orderId)
        var fos: FileOutputStream = null
        var isReader: InputStreamReader = null
        var reader: BufferedReader = null
        try {
          isReader = new InputStreamReader(is)
          reader = new BufferedReader(isReader)
          fos = new FileOutputStream(tmpFile)

          val bytes = (hash.value + '\n').getBytes
          fos.write(bytes)
          var line = reader.readLine()
          while (line != null) {
            fos.write((line + '\n').getBytes)
            line = reader.readLine()
          }

          val deleteFile = File.createTempFile("adept-order-delete", repository.name.value)
          fos.flush()
          orderFile.renameTo(deleteFile)
          tmpFile.renameTo(orderFile)
          deleteFile.delete()

          orderFile
        } finally {
          if (fos != null) fos.close()
          if (isReader != null) isReader.close()
          if (reader != null) reader.close()
        }
      case error => throw IllegalOrderStateException(repository, "Got " + error + " instead of Right(Some(...))")
    }
  }

  def activeVariants(id: Id, repository: GitRepository, commit: Commit): Set[VariantHash] = {
    chosenVariants(id, Set.empty, repository, commit)
  }

  def chosenVariants(id: Id, variants: Set[VariantHash], repository: GitRepository, commit: Commit): Set[VariantHash] = {
    var comparableVariants = variants
    val orderIds = listActiveOrderIds(id, repository, commit)
    orderIds.foreach { orderId =>
      repository.usingOrderInputStream(id, orderId, commit) {
        case Right(Some(is)) =>
          var foundVariantHash: Option[VariantHash] = None
          var first: Option[VariantHash] = None
          io.Source.fromInputStream(is).getLines.foreach { line =>
            if (first.isEmpty) {
              first = Some(VariantHash(line.trim()))
            }
            val currentHash = VariantHash(line)
            if (variants.contains(currentHash)) {
              if (foundVariantHash.isEmpty) foundVariantHash = Some(currentHash) //found a hash, is considered the best of which ever hashes are found in this file
              else if (foundVariantHash.isDefined) comparableVariants -= currentHash //there is already a hash which is considered better so remove this one
            }
          }
          if (foundVariantHash.isEmpty) comparableVariants ++= first //we did not find any matches in this file so pick first
        case error => throw IllegalOrderStateException(repository, "Got " + error + " instead of Right(Some(...))")
      }
    }
    comparableVariants
  }

  def replace(id: Id, orderId: OrderId, repository: GitRepository, commit: Commit)(block: VariantHash => Option[Seq[VariantHash]]): Option[File] = {
    repository.usingOrderInputStream(id, orderId, commit) {
      case Right(None) => None
      case Right(Some(is)) =>
        val tmpFile = File.createTempFile("adept-order-tmp-update-", repository.name.value)

        val fileWriter = new FileWriter(tmpFile)
        val isReader = new InputStreamReader(is)

        val writer = new BufferedWriter(fileWriter)
        val reader = new BufferedReader(isReader)

        try {
          var line = reader.readLine()
          var updated = false
          while (line != null) {
            block(VariantHash(line)) match {
              case Some(variants) =>
                variants.foreach { variant =>
                  writer.write(variant.value + '\n')
                }
                updated = true
              case None =>
                writer.write(line)
            }
            line = reader.readLine()
          }
          writer.flush()
          val orderFile = ""
          if (updated) {
            val deleteFile = File.createTempFile("adept-order-delete", repository.name.value)
            val orderFile = repository.getOrderFile(id, orderId)
            orderFile.renameTo(deleteFile)
            tmpFile.renameTo(orderFile)
            deleteFile.delete()
            Some(orderFile)
          } else {
            tmpFile.delete()
            None
          }
        } finally {
          fileWriter.close()
          isReader.close()
          writer.close()
        }
      case Left(error) =>
        throw new Exception("Could not read: " + id + " of order id: " + orderId + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }

  import Repository._
  import GitRepository._

  def listActiveOrderIds(id: Id, repository: GitRepository, commit: Commit): Set[OrderId] = {
    val orderPath = s"""$VariantsMetadataDirName$GitPathSep${id.value}"""
    val OrderIdExtractionRegEx = s"""$orderPath$GitPathSep$OrderFileNamePrefix(.*?)""".r
    val orderIds = repository.usePath[OrderId](Some(orderPath), commit) { path =>
      path match {
        case OrderIdExtractionRegEx(id) => Some(OrderId(id))
        case _ => None
      }
    }
    orderIds
  }

  /** get N order ids from start (could be size of active order ids)  */
  def getXOrderId(id: Id, repository: GitRepository, start: Int = 0, N: Int = 1): Set[OrderId] = {
    if (N < 1) throw new IllegalArgumentException("Cannot get " + N + " new order ids (n is smaller than 1)")
    if (start < 0) throw new IllegalArgumentException("Cannot start at " + start + " new order ids (start is smaller than 0)")

    var seed = id.value + {
      repository.usingRevWalk { (gitRepo, revWalk) =>
        val revCommit = repository.lookup(gitRepo, revWalk, InitTag)
          .getOrElse(throw new Exception("Cannot get next order because init tag: " + InitTag + " does not resolve a commit - is the repository " + repository.dir.getAbsolutePath + "not properly initialized?"))
        revCommit.name
      }
    }

    def createHash(lastSeed: String) = {
      Hasher.hash((lastSeed * 2 + 42).getBytes)
    }

    for (i <- 0 to start) {
      val lastSeed = seed
      seed = createHash(lastSeed)
    }
    (for (i <- 0 to N) yield {
      val lastSeed = seed
      seed = createHash(lastSeed)
      OrderId(seed)
    }).toSet
  }
}