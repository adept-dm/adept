package adept.repository.serialization

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

object MetadataContent {
  def write(string: String, file: File): File = {
    val fw = new FileWriter(file)
    val bw = new BufferedWriter(fw)
    try {
      bw.write(string)
      file
    } finally {
      bw.close()
      fw.close()
    }
  }
}