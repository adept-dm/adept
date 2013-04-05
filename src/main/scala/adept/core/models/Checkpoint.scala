package adept.core.models

import java.io._

object Checkpoint {
  def write(file: File, hash: Hash): Unit = {
    val writer = new FileWriter(file)
    try {
      writer.write(hash.value)
      writer.flush()
    } finally {
      writer.close()
    }
  }
  
  def read(file: File): Option[Hash] = {
    val source = io.Source.fromFile(file)
    try {
      source.getLines.take(1).toList.headOption.map(Hash.apply _)
    } finally {
      source.close()
    }
  }
}