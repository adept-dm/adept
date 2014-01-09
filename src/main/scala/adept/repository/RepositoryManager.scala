package adept.repository

import java.io.File
import adept.core.models._
import adept.core.resolution.VariantsLoaderEngine
import adept.core.resolution.VariantsLoaderLogic

class RepesitoryManager(repos: Set[Repository]) extends VariantsLoaderEngine(VariantsLoaderLogic.default) {

  def get(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    
    ???
  }
  
}