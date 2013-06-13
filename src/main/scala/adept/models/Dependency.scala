package adept.models

object Dependency {
  private[adept] val DefaultConf = ""
  
  private[adept] val defaultConfMapping: String => String = mapConf _
  
  private def mapConf(confs: String): String =  {
    confs.split(";").map{ 
      case "" => "*->*"
      case expr if expr.contains("->") => expr
      case expr => expr+"->"+expr
    }.mkString(",")
  }
  
}

case class Dependency(coords: Coordinates, hash: Hash, configuration: String = Dependency.DefaultConf) {
  
}
