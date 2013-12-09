package adept.core.models

case class Id(val value: String) extends AnyVal { //make a value class to avoid runtime reference
  override def toString = value
}
