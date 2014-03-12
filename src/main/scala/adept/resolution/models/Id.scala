package adept.resolution.models


/**
 * The unique Id representing a variant.
 * 
 * We use a value class to make methods more type-safe 
 * than using Strings, while avoiding the run-time cost.
 */
object Id {
  
  /** 
   * The separator used to nest variants with the same parts
   * E.g. foo/bar/cool is stored as in the directories foo/bar/cool  
   */
  val Sep = "/"
}

case class Id(val value: String) extends AnyVal { //make a value class to avoid runtime reference
  override def toString = value
}
