package adept.core.models

import java.sql.Timestamp

case class Commit(hash: Hash, version: Int, pushed: Option[Timestamp])