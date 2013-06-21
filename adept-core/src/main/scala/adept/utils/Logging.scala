package adept.utils

import org.slf4j.LoggerFactory

trait Logging {
  protected val logger = LoggerFactory.getLogger(this.getClass)
}
