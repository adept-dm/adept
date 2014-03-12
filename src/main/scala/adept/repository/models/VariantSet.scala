package adept.repository.models

import adept.repository.serialization.Order

/** Represents a complete set of variant hashes */
case class VariantSet(hashes: Set[VariantHash]) {
  override def toString = asLine

  def asLine = {
    hashes.toSeq.map(_.value).sorted.mkString(Order.SplitSymbol)
  }
}
