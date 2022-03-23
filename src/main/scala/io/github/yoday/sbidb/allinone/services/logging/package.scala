package io.github.yoday.sbidb.allinone.services

import io.github.yoday.sbidb.allinone.services.logging.ColoredConsoleAppender
import org.apache.log4j.{ConsoleAppender, Level, Logger, PatternLayout}
import org.slf4j.LoggerFactory
import zio.{Has, UIO, ULayer, URIO, ZIO, ZLayer}

// The zio-logging library is poorly documented and unreliable at the moment, so we need to do our own logging.
package object logging {

  trait ZLogger {

    def error(message: => String): UIO[Unit]

    def error(message: => String, throwable: => Throwable): UIO[Unit]

    def warn(message: => String): UIO[Unit]

    def warn(message: => String, throwable: => Throwable): UIO[Unit]

    def info(message: => String): UIO[Unit]

    def info(message: => String, throwable: => Throwable): UIO[Unit]

    def debug(message: => String): UIO[Unit]

    def trace(message: => String): UIO[Unit]

  }

  object Logging {

    def getLoggerForClass(clazz: Class[_]): URIO[Logging, ZLogger] = ZIO.accessM(_.get.getLoggerForClass(clazz))

  }

  type Logging = Has[LoggingService]

  trait LoggingService {

    def getLoggerForClass(clazz: Class[_]): UIO[ZLogger]

  }

  case class LoggingServiceLive() extends LoggingService {

    private class ZLoggerLive(private val logger: org.slf4j.Logger) extends ZLogger {

      override def error(message: => String): UIO[Unit] = ZIO.succeed {
        if (logger.isErrorEnabled) {
          logger.error(message)
        }
      }

      override def error(message: => String, throwable: => Throwable): UIO[Unit] = ZIO.succeed {
        if (logger.isErrorEnabled) {
          logger.error(message, throwable)
        }
      }

      override def warn(message: => String): UIO[Unit] = ZIO.succeed {
        if (logger.isWarnEnabled) {
          logger.warn(message)
        }
      }

      override def warn(message: => String, throwable: => Throwable): UIO[Unit] = ZIO.succeed {
        if (logger.isWarnEnabled) {
          logger.warn(message, throwable)
        }
      }

      override def info(message: => String): UIO[Unit] = ZIO.succeed {
        if (logger.isInfoEnabled) {
          logger.info(message)
        }
      }

      override def info(message: => String, throwable: => Throwable): UIO[Unit] = ZIO.succeed {
        if (logger.isInfoEnabled) {
          logger.info(message, throwable)
        }
      }

      override def debug(message: => String): UIO[Unit] = ZIO.succeed {
        if (logger.isDebugEnabled) {
          logger.debug(message)
        }
      }

      override def trace(message: => String): UIO[Unit] = ZIO.succeed {
        if (logger.isTraceEnabled) {
          logger.trace(message)
        }
      }
    }

    override def getLoggerForClass(clazz: Class[_]): UIO[ZLogger] = ZIO.succeed {
      new ZLoggerLive(LoggerFactory.getLogger(clazz))
    }

  }

  // FIXME: This initialization should probably be a ZIO.effect() but the application is
  //  hosed if it can't even setup logging without a failure.
  val LIVE: ULayer[Logging] = ZLayer.succeed {
    val layout = new PatternLayout
    layout.setConversionPattern("[%-5p] %d [%t] %c - %m%n")

    val consoleAppender = new ConsoleAppender
    //val consoleAppender = new ColoredConsoleAppender
    consoleAppender.setLayout(layout)
    consoleAppender.activateOptions()

    val rootLogger = Logger.getRootLogger
    rootLogger.setLevel(Level.INFO)
    rootLogger.addAppender(consoleAppender)

    Logger.getLogger("slick").setLevel(Level.WARN)
    Logger.getLogger("io.github.yoday.sbidb").setLevel(Level.DEBUG)
    //Logger.getLogger("slick.jdbc.StatementInvoker").setLevel(Level.DEBUG)


    LoggingServiceLive()
  }

}
