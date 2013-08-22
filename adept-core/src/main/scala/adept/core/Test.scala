package adept.core

class Test {

  def b = {
    1
  }
  
  def a: Unit = {
    
    "" match {
      case "" => b
      case _ => ""
    }
    
  }
}