package adept.core

trait Logger { //TODO: use a real logging framework
  def warn(msg: String) = {
    System.err.println("WARN: " + msg)
  }
}
