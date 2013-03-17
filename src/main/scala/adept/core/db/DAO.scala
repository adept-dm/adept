package adept.core.db

object DAO {
  val driver = slick.driver.H2Driver
  
  lazy val allDDLs = {
    import driver._
    Modules.ddl ++ Commits.ddl
  }
}
