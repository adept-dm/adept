package adept.core.db

object DAO {
  val driver = slick.driver.H2Driver
  
  lazy val mainDDLs = {
    import driver._
    Modules.ddl ++ Commits.ddl
  }
  
  lazy val stagedDDLs = {
    import driver._
    Modules.ddl ++ Checkpoints.ddl
  }
}
