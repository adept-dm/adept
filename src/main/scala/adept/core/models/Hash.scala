package adept.core.models

import java.net.URI
import java.io.InputStream
import java.io.File
import java.io.FileInputStream
import java.security.MessageDigest

case class Hash(val value: String) extends AnyVal { //make a value class to avoid runtime reference
  override def toString = value
}

object Hash {
  private lazy val md: ThreadLocal[MessageDigest] = new ThreadLocal[MessageDigest] { //make message digest thread-"safe"
    override def initialValue() = {
      MessageDigest.getInstance("SHA-256")
    }
  }

  private def encode(bytes: Array[Byte]) = {
    md.get().digest(bytes).map(b => "%02X" format b).mkString.toLowerCase
  }

  private def updateWithConstraint(constraint: Constraint, currentMd: MessageDigest) = {
    currentMd.update(constraint.name.getBytes)
    constraint.values.foreach { value =>
      currentMd.update(value.getBytes)
    }
  }

  private def updateWithArtifactRef(artifactRef: ArtifactRef, currentMd: MessageDigest) = {
    currentMd.update(artifactRef.hash.value.getBytes)
    artifactRef.attributes.foreach(updateWithAttribute(_, currentMd))
  }

  private def updateWithAttribute(attribute: Attribute, currentMd: MessageDigest) = {
    currentMd.update(attribute.name.getBytes)
    attribute.values.foreach { value =>
      currentMd.update(value.getBytes)
    }
  }

  private def updateWithDependency(dependency: Dependency, currentMd: MessageDigest) = {
    currentMd.update(dependency.id.value.getBytes)
    dependency.constraints.foreach(updateWithConstraint(_, currentMd))
  }

  private def updateWithVariant(variant: Variant, currentMd: MessageDigest): Unit = {
    currentMd.update(variant.id.value.getBytes)
    variant.dependencies.foreach(updateWithDependency(_, currentMd))
    variant.artifacts.foreach(updateWithArtifactRef(_, currentMd))
    variant.attributes.foreach(updateWithAttribute(_, currentMd))
  }

  def calculate(dependencies: Set[Dependency]): Hash = {
    val currentMd = md.get()
    currentMd.reset()
    try {
      dependencies.foreach(updateWithDependency(_, currentMd))
      Hash(currentMd.digest().map(b => "%02X" format b).mkString.toLowerCase)
    } finally {
      currentMd.reset()
    }
  }

  def calculate(variant: Variant): Hash = {
    val currentMd = md.get() /* thread-safe because of thread local */
    currentMd.reset()
    try {
      updateWithVariant(variant, currentMd)

      Hash(currentMd.digest().map(b => "%02X" format b).mkString.toLowerCase)
    } finally {
      currentMd.reset()
    }
  }

  def calculate(variants: Seq[Variant]): Hash = {
    val currentMd = md.get() /* thread-safe because of thread local */
    currentMd.reset()
    try {
      variants.foreach { variant =>
        updateWithVariant(variant, currentMd)
      }
      Hash(currentMd.digest().map(b => "%02X" format b).mkString.toLowerCase)

    } finally {
      currentMd.reset()
    }
  }

  def calculate(in: InputStream): Hash = {
    val currentMd = md.get()
    currentMd.reset()
    val buf = new Array[Byte](1024 * 4) //_seemed_ to be the fastest when I tried it out when I was writing this
    try {
      var len = in.read(buf)

      while (len > 0) {
        currentMd.update(buf, 0, len)
        len = in.read(buf)
      }
      //was streaming is about 4 times more efficient than using digest on Array[Byte]
      Hash(currentMd.digest().map(b => "%02X" format b).mkString.toLowerCase)
    } finally {
      currentMd.reset()
      in.close
    }
  }

  def calculate(file: File): Hash = {
    calculate(new FileInputStream(file))
  }

}
