package io.github.yoday.sbidb.allinone

import io.github.yoday.sbidb.allinone.repositories.player_repo._
import io.github.yoday.sbidb.allinone.repositories.team_repo._
import io.github.yoday.sbidb.allinone.repositories.game_repo._
import io.github.yoday.sbidb.allinone.repositories.{game_repo, player_repo, team_repo}
import io.github.yoday.sbidb.allinone.services._
import io.github.yoday.sbidb.allinone.services.app_configuration.AppConfiguration
import io.github.yoday.sbidb.allinone.services.data_initializer.DataInitializer
import io.github.yoday.sbidb.allinone.services.elasticsearch.Elasticsearch
import io.github.yoday.sbidb.allinone.services.logging.Logging
import io.github.yoday.sbidb.allinone.services.mustache.Mustache
import io.github.yoday.sbidb.allinone.services.search_engine.SearchEngine
import io.github.yoday.sbidb.allinone.services.slick.Slick
import io.github.yoday.sbidb.allinone.views.player_view._
import io.github.yoday.sbidb.allinone.views.team_view._
import io.github.yoday.sbidb.allinone.views.game_view._
import io.github.yoday.sbidb.allinone.views.search_result_view.SearchResultView
import io.github.yoday.sbidb.allinone.views.{game_view, health_checks, home_view, player_view, search_result_view, team_view}
import zhttp.service.{EventLoopGroup, Server}
import zhttp.service.server.ServerChannelFactory
import zio._

object Main extends zio.App {

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    val appLayers = {
      logging.LIVE >+>
      app_configuration.LIVE >+>
      postgres.LIVE >+>
      slick.LIVE >+>
      elasticsearch.LIVE >+>
      schema_builder.LIVE >+>
      player_repo.LIVE >+>
      team_repo.LIVE >+>
      game_repo.LIVE >+>
      mustache.LIVE >+>
      player_view.LIVE >+>
      team_view.LIVE >+>
      game_view.LIVE >+>
      search_engine.LIVE >+>
      search_result_view.LIVE >+>
      data_initializer.LIVE
    }

    val appRoutes = {
      home_view.ROUTES ++
      player_view.ROUTES ++
      team_view.ROUTES ++
      game_view.ROUTES ++
      search_result_view.ROUTES ++
      health_checks.ROUTES
    }

    // Scalac is not able to infer the right types
    type RemainingServerLayers =
      Mustache
      with Slick
      with Elasticsearch
      with SearchEngine
      with PlayerRepo with PlayerView
      with TeamRepo with TeamView
      with GameRepo with GameView
      with SearchResultView

    // TODO: Refactor this to use the config values of the app_configuration module.
    val server = (
      Server.paranoidLeakDetection ++
      Server.port(9080) ++
      Server.app(appRoutes)
    ).make.provideSomeLayer[RemainingServerLayers](ServerChannelFactory.auto ++ EventLoopGroup.auto(4))

    val app = for {
      pgPort <- AppConfiguration.getPostgresConfig.map(_.port)
      esPort <- AppConfiguration.getElasticsearchConfig.map(_.port)
      logger <- Logging.getLoggerForClass(this.getClass)
      _ <- DataInitializer.run
      _ <- PortKiller.run(9080)
      _ <- server.use { start =>
        val msg = s"The server is now up. Postgres is running on port $pgPort. ElasticSearch is running on $esPort. Go to http://localhost:${start.port}/ to view the site."
        logger.info(msg) *> ZIO.never
      }
    } yield ()

    app.provideCustomLayer(appLayers).exitCode
  }

}
