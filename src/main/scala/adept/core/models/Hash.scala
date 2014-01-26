package adept.core.models

import java.net.URI
import java.io.InputStream
import java.io.File
import java.io.FileInputStream
import java.security.MessageDigest
import adept.configuration.ConfigurationId

case class Hash(val value: String) extends AnyVal { //make a value class to avoid runtime reference
  override def toString = value
}

object Hash {
  import Ordering._

  //TODO: there is not point in this? just create a local varialbe instead
  private lazy val md: ThreadLocal[MessageDigest] = new ThreadLocal[MessageDigest] { //make message digest thread-"safe"
    override def initialValue() = {
      MessageDigest.getInstance("SHA-256")
    }
  }

  private def updateWithConstraint(constraint: Constraint, currentMd: MessageDigest) = {
    currentMd.update(constraint.name.getBytes)
    constraint.values.toSeq.sorted.foreach { value =>
      currentMd.update(value.getBytes)
    }
  }

  private def updateWithArtifactRef(artifactRef: ArtifactRef, currentMd: MessageDigest) = {
    currentMd.update(artifactRef.hash.value.getBytes)
    artifactRef.attributes.toSeq.sorted.foreach(updateWithAttribute(_, currentMd))
  }

  private def updateWithAttribute(attribute: Attribute, currentMd: MessageDigest) = {
    currentMd.update(attribute.name.getBytes)
    attribute.values.toSeq.sorted.foreach { value =>
      currentMd.update(value.getBytes)
    }
  }

  private def updateWithIdConstraints(id: Id, constraints: Set[Constraint], currentMd: MessageDigest) = {
    currentMd.update(id.value.getBytes)
    constraints.toSeq.sorted.foreach(updateWithConstraint(_, currentMd))
  }

  private def updateWithDependency(dependency: Dependency, currentMd: MessageDigest) = {
    updateWithIdConstraints(dependency.id, dependency.constraints, currentMd)
  }

  private def updateWithVariant(variant: Variant, currentMd: MessageDigest): Unit = {
    currentMd.update(variant.id.value.getBytes)
    variant.dependencies.toSeq.sorted.foreach(updateWithDependency(_, currentMd))
    variant.artifacts.toSeq.sorted.foreach(updateWithArtifactRef(_, currentMd))
    variant.attributes.toSeq.sorted.foreach(updateWithAttribute(_, currentMd))
  }

  private def digest(currentMd: MessageDigest) = currentMd.digest().map(b => "%02x" format b).mkString

  def calculate(id: Id, constraints: Set[Constraint]): Hash = {
    val currentMd = md.get()
    currentMd.reset()
    try {
      updateWithIdConstraints(id, constraints, currentMd)

      Hash(digest(currentMd))
    } finally {
      currentMd.reset()
    }
  }

  def calculate(dependencies: Set[Dependency]): Hash = {
    val currentMd = md.get()
    currentMd.reset()
    try {
      dependencies.toSeq.sorted.foreach(updateWithDependency(_, currentMd))

      Hash(digest(currentMd))
    } finally {
      currentMd.reset()
    }
  }
  
  def calculate(attributes: Set[Attribute], configurations: Set[ConfigurationId]): Hash = {
    val currentMd = md.get()
    currentMd.reset()
    try {
      attributes.toSeq.sorted.foreach(updateWithAttribute(_, currentMd))
      configurations.map(_.value).toSeq.sorted.foreach{ confName =>
        currentMd.update(confName.getBytes)
      }
      Hash(digest(currentMd))
    } finally {
      currentMd.reset()
    }
  }

  def calculate(variant: Variant): Hash = {
    val currentMd = md.get() /* thread-safe because of thread local */
    currentMd.reset()
    try {
      updateWithVariant(variant, currentMd)

      Hash(digest(currentMd))
    } finally {
      currentMd.reset()
    }
  }

  @deprecated("will be removed, since it is not strictly needed")
  def calculate(variants: Seq[Variant]): Hash = {
    val currentMd = md.get() /* thread-safe because of thread local */
    currentMd.reset()
    try {
      System.err.println("no realiable hash!")
      variants.foreach(updateWithVariant(_, currentMd))

      Hash(digest(currentMd))
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
      //streaming was much more efficient than using digest on Array[Byte] - to be verified...
      Hash(digest(currentMd))
    } finally {
      currentMd.reset()
      in.close
    }
  }

  def calculate(file: File): Hash = {
    val fis = new FileInputStream(file)
    try {
      calculate(fis)
    } finally {
      fis.close()
    }
  }
}

