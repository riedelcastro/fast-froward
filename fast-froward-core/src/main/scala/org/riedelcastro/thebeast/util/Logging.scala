package org.riedelcastro.thebeast.util

/**
 * @author sriedel
 */

trait Logging {
  import Logging._
  def info(text: => String) = if (isActive(INFO)) logger(INFO, text, getClass)

  def debug(text: => String) = if (isActive(DEBUG)) logger(DEBUG, text, getClass)

}

object Logging {
  case class Level(name: String, level: Int)
  object DEBUG extends Level("DEBUG", 0)
  object TRACE extends Level("TRACE", 1)
  object INFO extends Level("INFO", 2)
  object WARN extends Level("WARN", 3)

  var level: Level = INFO

  def isActive(level: Level) = level.level >= this.level.level

  var logger: (Level, String, Class[_]) => Unit = (level, text, clazz) => {
    println("%s@%s: %s".format(level.name,clazz.getName, text))
  }
  
}