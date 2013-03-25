package adept.core.models

case class Change(module: Module, commitHash: Option[Hash], deleted: Boolean) 
