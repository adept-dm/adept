package adept.repository.models

/**
 * Value class of (Git) Commit.
 * 
 * We use a value class to make methods more type-safe 
 * than using Strings, while avoiding the run-time cost.
 */
case class Commit(value: String) extends AnyVal {
  override def toString = value
}