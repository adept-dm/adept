package adept.models

object Dependency {
  private[models] val DefaultConf = ""
  
  private[models] val defaultConfMapping: String => String = mapConf _
  
  private def mapConf(confs: String): String =  {
    confs.split(",").map{ 
      case "" => "*->*"
      case expr if expr.contains("->") => expr
      case expr => expr+"->"+expr
    }.mkString(",")
  }
}

case class Dependency(coords: Coordinates, hash: Hash, configuration: String = Dependency.DefaultConf) {
  
}
