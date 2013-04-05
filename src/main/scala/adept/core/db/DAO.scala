package adept.core.db

object DAO {
  val driver = slick.driver.H2Driver
  
  val mainTables = {
    Seq(Modules, Commits)
  }
  
  lazy val mainDDLs = {
    import driver._
    mainTables.tail.foldLeft(mainTables.head.ddl)((a,b) => a ++ b.ddl) //fails hard if there is no tables, but that is ok?
  }
  
  lazy val stagedDDLs = {
    import driver._
    Modules.ddl
  }
}
