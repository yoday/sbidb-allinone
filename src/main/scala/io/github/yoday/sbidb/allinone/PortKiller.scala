package io.github.yoday.sbidb.allinone

import io.github.yoday.sbidb.allinone.services.logging.Logging
import zio.{RIO, ZIO}

import scala.util.Try
import scala.util.control.NonFatal
import sys.process._

object PortKiller {

  // FIXME: Need to make this more portable or find a way to make the postgres and elasticsearch boots more durable.
  def run(port: Int): RIO[Logging, Unit] = {
    for {
      logger <- Logging.getLoggerForClass(this.getClass)
      _ <- logger.info(s"Starting port killing for port=$port")
      lines <- ZIO.effect[String] {
        try {
          s"lsof -i tcp:$port".!!
        } catch {
          case NonFatal(err) =>
            // this whole block is a hack, we don't care about failures
            logger.error(s"Got some exception during PortKiller.run(port=$port). This error can be ignored.", err)
            ""
        }
      }
      _ <- logger.info(s"port scan result: $lines")
      pids <- ZIO.effect[Set[Int]] {
        try {
          lines.split('\n')
            .drop(1)
            .flatMap {
              _.split("\\s+").slice(1, 2)
            }
            .flatMap { x => Try(x.toInt).toOption }
            .toSet
        } catch {
          case NonFatal(err) =>
            // this whole block is a hack, we don't care about failures
            logger.error(s"Got some exception during PortKiller.run(port=$port). This error can be ignored.", err)
            Set.empty
        }
      }
      _ <- logger.debug(s"pids computed: $pids")
      _ <- ZIO.effect {
        try {
          pids.foreach { pid =>
            s"kill -9 $pid".!
          }
        } catch {
          case NonFatal(err) =>
          // this whole block is a hack, we don't care about failures
            logger.error(s"Got some exception during PortKiller.run(port=$port). This error can be ignored.", err)
        }
      }
      _ <- logger.info(s"Finished port killing for port=$port")
    } yield ()
  }

}
