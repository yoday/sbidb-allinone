package io.github.yoday.sbidb.allinone.services.logging

import org.apache.log4j.ConsoleAppender
import org.apache.log4j.spi.LoggingEvent

class ColoredConsoleAppender extends ConsoleAppender {

  final val COLOR_FG_BLACK    = "\u001b[0;30"
  final val COLOR_FG_RED      = "\u001b[0;31"
  final val COLOR_FG_GREEN    = "\u001b[0;32"
  final val COLOR_FG_YELLOW   = "\u001b[0;33"
  final val COLOR_FG_BLUE     = "\u001b[0;34"
  final val COLOR_FG_MAGENTA  = "\u001b[0;35"
  final val COLOR_FG_CYAN     = "\u001b[0;36"
  final val COLOR_FG_WHITE    = "\u001b[0;37"

  final val COLOR_RESET       = "\u001b[m"

  final val COLOR_LEVEL_FATAL = COLOR_FG_MAGENTA
  final val COLOR_LEVEL_ERROR = COLOR_FG_RED
  final val COLOR_LEVEL_WARN  = COLOR_FG_YELLOW
  final val COLOR_LEVEL_INFO  = COLOR_FG_WHITE
  final val COLOR_LEVEL_DEBUG = COLOR_FG_CYAN
  final val COLOR_LEVEL_TRACE = COLOR_FG_BLUE

  override def subAppend(event: LoggingEvent): Unit = {
    val maybeColor = determineColor(event)
    maybeColor.foreach { color => qw.write(color) }
    super.subAppend(event)
    maybeColor.foreach { _ => qw.write(COLOR_RESET) }
    qw.flush()
  }

  protected def determineColor(event: LoggingEvent): Option[String] = {
    event.getLevel.toString match {
      case "FATAL"  => Some(COLOR_LEVEL_FATAL)
      case "ERROR"  => Some(COLOR_LEVEL_ERROR)
      case "WARN"   => Some(COLOR_LEVEL_WARN)
      case "INFO"   => Some(COLOR_LEVEL_INFO)
      case "DEBUG"  => Some(COLOR_LEVEL_DEBUG)
      case "TRACE"  => Some(COLOR_LEVEL_TRACE)
      case _ => None
    }
  }

}
