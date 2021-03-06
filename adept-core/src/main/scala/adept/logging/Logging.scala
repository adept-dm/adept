package adept.logging

import org.slf4j.LoggerFactory

trait Logging {
  protected lazy val logger = LoggerFactory.getLogger(this.getClass) //must be lazy because underlying logging system must be configured
}