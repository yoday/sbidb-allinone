package io.github.yoday.sbidb.allinone.services

import javax.sql.DataSource
import io.github.yoday.sbidb.allinone.PortKiller
import io.github.yoday.sbidb.allinone.services.app_configuration._
import io.github.yoday.sbidb.allinone.services.logging.Logging
import io.zonky.test.db.postgres.embedded.EmbeddedPostgres
import zio.{Has, RIO, RLayer, Task, ZIO, ZLayer}

package object postgres {

  object Postgres {

    def getDataSource: RIO[Postgres, DataSource] = ZIO.accessM(_.get.getDataSource)

  }

  type Postgres = Has[PostgresService]

  trait PostgresService {

    def getDataSource: Task[DataSource]

  }

  case class PostgresServiceLive(epg: EmbeddedPostgres) extends PostgresService {

    override def getDataSource: Task[DataSource] = {
      ZIO.effect(epg.getPostgresDatabase)
    }

  }

  // TODO: Figure out a cleaner way to initialize the service; perhaps using ZManaged.
  val LIVE: RLayer[AppConfiguration with Logging, Postgres] = ZLayer.fromServiceM { appConfiguration: AppConfigurationService =>
    for {
      config <- appConfiguration.getPostgresConfig
      logger <- Logging.getLoggerForClass(this.getClass)
      _ <- logger.info("Postgres bootup started")
      _ <- PortKiller.run(config.port)
      epg <- ZIO.effect {
        // FIXME: We should be using blocking.effectBlocking() but the Blocking type is causing issues building the layer cake.
        EmbeddedPostgres.builder
          .setCleanDataDirectory(config.wipeDataOnStartup)
          //.setDataDirectory("postgres-data")
          .setPort(config.port)
          .start()
      }
      _ <- logger.info("Postgres bootup finished")
    } yield PostgresServiceLive(epg)
  }

}
