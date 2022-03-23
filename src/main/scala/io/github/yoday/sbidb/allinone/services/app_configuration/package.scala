package io.github.yoday.sbidb.allinone.services

import zio.{Has, UIO, ULayer, URIO, ZIO, ZLayer}

package object app_configuration {

  case class HttpServerConfig(
    port: Int,
    numThreads: Int
  )

  case class PostgresConfig(
    port: Int,
    wipeDataOnStartup: Boolean
  )

  case class ElasticsearchConfig (
    port: Int,
    esVersion: String,
    clusterName: String,
    wipeDataOnStartup: Boolean
  )

  object AppConfiguration {

    def getHttpServerConfig: URIO[AppConfiguration, HttpServerConfig] = ZIO.accessM(_.get.getHttpServerConfig)

    def getPostgresConfig: URIO[AppConfiguration, PostgresConfig] = ZIO.accessM(_.get.getPostgresConfig)

    def getElasticsearchConfig: URIO[AppConfiguration, ElasticsearchConfig] = ZIO.accessM(_.get.getElasticsearchConfig)

  }

  type AppConfiguration = Has[AppConfigurationService]

  trait AppConfigurationService {

    def getHttpServerConfig: UIO[HttpServerConfig]

    def getPostgresConfig: UIO[PostgresConfig]

    def getElasticsearchConfig: UIO[ElasticsearchConfig]

  }

  // TODO: We should load the settings from a mix of config files and command-line options as a future enhancement.
  case class AppConfigurationServiceLive() extends AppConfigurationService {

    override def getHttpServerConfig: UIO[HttpServerConfig] = ZIO.succeed {
      HttpServerConfig(
        port = 9080,
        numThreads = 4
      )
    }

    override def getPostgresConfig: UIO[PostgresConfig] = ZIO.succeed {
      PostgresConfig(
        port = 9432,
        wipeDataOnStartup = true
      )
    }

    override def getElasticsearchConfig: UIO[ElasticsearchConfig] = ZIO.succeed {
      ElasticsearchConfig(
        port = 9200,
        esVersion = "6.0.0",
        clusterName = "sbidb",
        wipeDataOnStartup = true
      )
    }

  }

  val LIVE: ULayer[AppConfiguration] = ZLayer.succeed(AppConfigurationServiceLive())

}
