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
import adept.repository.models.VariantSet
import java.io.BufferedReader
import java.io.Reader
import java.io.InputStreamReader
import java.io.FileReader
import java.io.BufferedWriter

case class FilePosition(foundMatch: VariantSet, lineNo: Long)

/**
 * Used to manage the variant order for a repository.
 *
 * Contains lines of sets of variant hashes.
 * In Adept, only one set of variant will be used for resolution.
 * The first line is considered to be the "latest" sets of variants.
 * Can find the set of variants that can be used together at any given time.
 *
 * Example:
 * Imagine if the following (version,binary-version) tuples corresponds to variant hashes:
 * (1.0.2,1.0);(1.1.0,1.1);(2.0.0,2.0) 
 * (1.0.1,1.0);(1.1.0,1.1);(2.0.0,2.0) 
 * (1.0.0,1.0);(1.1.0,1.1);(2.0.0,2.0) 
 * (1.0.2,1.0);(1.1.1,1.1);(2.0.0,2.0) 
 * (1.0.1,1.0);(1.1.1,1.1);(2.0.0,2.0) 
 * (1.0.0,1.0);(1.1.1,1.1);(2.0.0,2.0)
 * (1.0.2,1.0);(1.1.0,1.1);(2.0.0,2.0) 
 * (1.0.1,1.0);(1.1.0,1.1);(2.0.0,2.0) 
 * (1.0.0,1.0);(1.1.0,1.1);(2.0.0,2.0)
 */
object Order {
  val SplitSymbol = ";"

  private def splitLine(line: String) = {
    line.split(SplitSymbol)
  }

  def add(id: Id, hashes: VariantSet, repository: Repository): File = {
    val tmpFile = File.createTempFile("adept-order-tmp-add-", repository.name.value)
    val orderFile = repository.getOrderFile(id)
    var fos: FileOutputStream = null
    var fis: FileInputStream = null

    try {
      fos = new FileOutputStream(tmpFile)
      if (orderFile.isFile)
        fis = new FileInputStream(orderFile)

      val bytes = (hashes.asLine + "\n").getBytes
      fos.write(bytes)

      val deleteFile = if (fis != null) {
        fis.getChannel().transferTo(0, Long.MaxValue, fos.getChannel)
        val deleteFile = File.createTempFile("adept-order-delete", repository.name.value)
        fos.flush()
        orderFile.renameTo(deleteFile)
        deleteFile
      } else null
      tmpFile.renameTo(orderFile)

      if (deleteFile != null) deleteFile.delete()
      orderFile
    } finally {
      if (fos != null) fos.close()
      if (fis != null) fis.close()
    }
  }

  private def lineHasMatch(line: String, hashesString: Set[String]): Boolean = {
    splitLine(line).exists { hash =>
      hashesString.contains(hash.trim())
    }
  }

  private def toHashes(line: String) = {
    splitLine(line).map(h => h.trim()).toSet
  }

  def firstMatch(id: Id, hashes: Set[VariantHash], repository: GitRepository, commit: Commit): Option[FilePosition] = {
    val hashesString = hashes.map(_.value)
    repository.usingOrderInputStream(id, commit) {
      case Right(None) => None
      case Right(Some(is)) =>
        var lineNo = -1 //start at 0
        io.Source.fromInputStream(is).getLines.find { line =>
          lineNo += 1
          if (!line.startsWith("#")) {
            lineHasMatch(line, hashesString)
          } else false
        }.map { line =>
          FilePosition(VariantSet(toHashes(line).map(VariantHash(_))), lineNo)
        }
      case Left(error) =>
        throw new Exception("Could not read: " + id + " and " + hashes + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }

  def update(id: Id, repository: GitRepository, commit: Commit)(block: VariantSet => Option[Seq[VariantSet]]): Option[File] = {
    repository.usingOrderInputStream(id, commit) {
      case Right(None) =>
        None
      case Right(Some(is)) =>
        val tmpFile = File.createTempFile("adept-order-tmp-update-", repository.name.value)
        val orderFile = repository.getOrderFile(id)

        val fileWriter = new FileWriter(tmpFile)
        val fileReader = new FileReader(orderFile)

        val writer = new BufferedWriter(fileWriter)
        val reader = new BufferedReader(fileReader)

        try {
          var line = reader.readLine()
          var updated = false
          while (line != null) {
            block(VariantSet(toHashes(line).map(VariantHash(_)))) match {
              case Some(variantSets) =>
                variantSets.foreach { variantSet =>
                  writer.write(variantSet.asLine + '\n')
                }
                updated = true
              case None =>
                writer.write(line)
            }
            line = reader.readLine()
          }
          writer.flush()
          if (updated) {
            val deleteFile = File.createTempFile("adept-order-delete", repository.name.value)
            orderFile.renameTo(deleteFile)
            tmpFile.renameTo(orderFile)
            deleteFile.delete()
          } else {
            tmpFile.delete()
          }
          Some(orderFile)
        } finally {
          fileWriter.close()
          fileReader.close()
          writer.close()
          reader.close()
        }
      case Left(error) =>
        throw new Exception("Could not read: " + id + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }
}