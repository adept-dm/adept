package adept.repository.models.configuration

import adept.models.Id

object ConfigurationId {
  
  val ConfigurationIdName = "config"
  
  def join(id: Id, configurationId: ConfigurationId): Id = {
      Id(id.value + Id.Sep + ConfigurationIdName + Id.Sep + configurationId.value)
  }
  
  def split(id: Id): Option[(Id, ConfigurationId)] = {
    id.value.split(Id.Sep + ConfigurationIdName + Id.Sep).toSeq match {
      case head +: tail +: Nil => Some(Id(head) -> ConfigurationId(tail))
      case _ => None 
    }
  }
}

case class ConfigurationId(value: String) extends AnyVal