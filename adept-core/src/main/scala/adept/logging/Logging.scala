package adept.logging

import org.slf4j.LoggerFactory

trait Logging {
  //must be lazy because underlying logging system must be configured
  protected lazy val logger = LoggerFactory.getLogger(this.getClass)
}
