package adept.client

import adept.repository._

object Parsers {
  val CoordsExpr = """\s*(.*):(.*):(.*)\s*""".r
  val CoordsMetadataExpr = """\s*(.*?)(\[(.*)\])?\s*""".r
  val MetadataExpr = """\s*(.*)=(.*)\s*""".r
  val CoordsMetadataHashExpr = """\s*(.*)@(.*)\s*""".r
  
  def descriptor(string: String): Either[String, Descriptor] = {
    string match {
      case CoordsMetadataHashExpr(coordsMeta, hash) => {
        coordsMetadata(coordsMeta).right.map{ case (coords, meta) =>
          Descriptor(coords, meta, Hash(hash))
        }
      }
      case noHash => Left(s"could not find required hash in $noHash")
    }
  }
  
  def coords(string: String): Either[String, Coordinates] = string match {
    case CoordsExpr(org, name, version) => {
      if (org.contains(" ") || name.contains(" ") || version.contains(" "))
        Left(s"could not parse coordinates because of whitespace in expression: '$string'")
      else
        Right(Coordinates(org, name, version))
    }
    case noCoords => Left(s"could not parse coordinates: $noCoords")
  }
  
  def metadata(string: String): Either[String, Metadata] = {
    val foundMetaEntries = string.split(",").map{ entry =>
      entry match {
        case MetadataExpr(key, value) => Right(key -> value)
        case noMeta => Left(s"could not parse metadata $string because of syntax error around: '$noMeta'")
      }
    }
    foundMetaEntries.foldLeft[Either[String, Metadata]](Right(Metadata(Map.empty))){ (product, current) =>
      for {
        p <- product.right
        c <- current.right
      } yield {
        Metadata(p.data + c)
      }
    }
  }
  
  def coordsMetadata(string: String): Either[String, (Coordinates, Metadata)] = string match {
    case CoordsMetadataExpr(coordsString, _, metadataString) =>
      val foundCoords = Parsers.coords(coordsString)
      val foundMetadata = (for {
        metadata <- Option(metadataString)
      } yield {
        Parsers.metadata(metadata)
      }).getOrElse{ Right(Metadata(Map.empty)) }
      for {
        coords <- foundCoords.right
        metadata <- foundMetadata.right
      } yield {
        coords -> metadata
      }
    case somethingElse => Left(s"coult not parse coordinates in: $somethingElse")
  } 
}
