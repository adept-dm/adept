package adept.core.db

import adept.core.models._

package object Types {  
  type ModulesType = (String, String, String, String, String, String, String, String, Option[String], Boolean)
  type RepositoryVersionsType = (String, Int)
  type ModuleRowType = (Module, Option[String], Boolean)  
}

