package io.github.yoday.sbidb.allinone.services

import io.github.yoday.sbidb.allinone.models._
import com.sksamuel.elastic4s.http.{ElasticClient, ElasticNodeEndpoint, ElasticProperties}
import io.github.yoday.sbidb.allinone.PortKiller
import io.github.yoday.sbidb.allinone.services.app_configuration._
import io.github.yoday.sbidb.allinone.services.logging.Logging
import io.github.yoday.sbidb.allinone.services.schema_builder.ESDocument
import pl.allegro.tech.embeddedelasticsearch.{EmbeddedElastic, PopularProperties}
import zio.json.ast.Json
import zio.json.{EncoderOps, JsonCodec}
import zio.{Has, RIO, RLayer, Task, URIO, ZIO, ZLayer}

package object elasticsearch {

  object dsl extends com.sksamuel.elastic4s.http.ElasticDsl

  type ESSearchRequest = com.sksamuel.elastic4s.searches.SearchRequest
  type ESSearchResponse = com.sksamuel.elastic4s.http.search.SearchResponse
  type ESSetupIndexRequest = com.sksamuel.elastic4s.indexes.CreateIndexRequest
  type ESSetupIndexResponse = com.sksamuel.elastic4s.http.index.CreateIndexResponse
  type ESIndexDocumentRequest = com.sksamuel.elastic4s.indexes.IndexRequest
  type ESIndexDocumentResponse = com.sksamuel.elastic4s.http.index.IndexResponse

  object Elasticsearch {

    def getClusterStatus: RIO[Elasticsearch, String] = ZIO.accessM(_.get.getClusterStatus)

    def getClusterVersion: RIO[Elasticsearch, String] = ZIO.accessM(_.get.getClusterVersion)

    def getDocument(document: ESDocument, docId: String): RIO[Elasticsearch, Option[Json]] = ZIO.accessM(_.get.getDocument(document, docId))

  }

  type Elasticsearch = Has[ElasticsearchService]

  trait ElasticsearchService {

    def getClusterStatus: Task[String]

    def getClusterVersion: Task[String]

    def showRequest(req: ESSearchRequest): Task[String]

    def showRequest(req: ESSetupIndexRequest): Task[String]

    def executeSearch(req: ESSearchRequest): Task[ESSearchResponse]

    def setupIndex(req: ESSetupIndexRequest): Task[ESSetupIndexResponse]

    def indexDocument(req: ESIndexDocumentRequest): Task[ESIndexDocumentResponse]

    def refreshIndex(document: ESDocument): Task[Unit]

    def getDocument(document: ESDocument, docId: String): Task[Option[Json]]

  }

  case class ElasticsearchServiceLive(ees: EmbeddedElastic, esc: ElasticClient) extends ElasticsearchService {
    import dsl._

    override def getClusterStatus: Task[String] = ZIO.fromFuture { implicit ec =>
      esc.execute {
        clusterHealth()
      }
    }.map { _.result.status }

    override def getClusterVersion: Task[String] = ZIO.fromFuture { implicit ec =>
      esc.execute {
        nodeInfo()
      }
    }.map { _.result.nodes.collect {
      case (_, nodeInfo) => nodeInfo.version
    }.toList.sorted.distinct.mkString(",") }

    // TODO: Refactor to be dump the request state better and to DRY.
    override def showRequest(req: ESSearchRequest): Task[String] = ZIO.effect {
      val regex = "(.+)\nStringEntity\\((.*),Some\\(application/json\\)\\)".r
      esc.show(req) match {
        case regex(header, body) =>
          val prettyBody = jsonJsonCodec.decodeJson(body).fold(_ => body, json => jsonJsonCodec.encodeJson(json, Some(0)).toString)
          s"$header\n$prettyBody"
        case x =>
          x
      }
    }

    // TODO: Refactor to be dump the request state better and to DRY.
    override def showRequest(req: ESSetupIndexRequest): Task[String] = ZIO.effect {
      val regex = "(.+)\nStringEntity\\((.*),Some\\(application/json\\)\\)".r
      esc.show(req) match {
        case regex(header, body) =>
          val prettyBody = jsonJsonCodec.decodeJson(body).fold(_ => body, json => jsonJsonCodec.encodeJson(json, Some(0)).toString)
          s"$header\n$prettyBody"
        case x =>
          x
      }
    }

    override def executeSearch(req: ESSearchRequest): Task[ESSearchResponse] = ZIO.fromFuture { implicit ec =>
      esc.execute(req).map(_.result)
    }

    override def setupIndex(req: ESSetupIndexRequest): Task[ESSetupIndexResponse] = ZIO.fromFuture { implicit ec =>
      esc.execute(req).map(_.result)
    }

    override def indexDocument(req: ESIndexDocumentRequest): Task[ESIndexDocumentResponse] = ZIO.fromFuture { implicit ec =>
      esc.execute(req).map(_.result)
    }

    override def refreshIndex(document: ESDocument): Task[Unit] = ZIO.fromFuture { implicit ec =>
      esc.execute(dsl.refreshIndex(document.index.index)).map(_.result)
    }

    override def getDocument(document: ESDocument, docId: String): Task[Option[Json]] = {
      for {
        maybeResult <- ZIO.fromFuture { implicit ec =>
          esc.execute(
            get(document.index.index, document.index.`type`, docId)
          ).map(_.result).map { res =>
            if (res.found) {
              Option(res.sourceAsString)
            } else {
              None
            }
          }
        }
        // FIXME: correctly handle json parsing errors
        result <- ZIO.effect {
          maybeResult.map { resultDoc =>
            jsonJsonCodec.decodeJson(resultDoc).right.get
          }
        }
      } yield result
    }
  }

  // TODO: Figure out a cleaner way to initialize the service; perhaps using ZManaged.
  val LIVE: RLayer[AppConfiguration with Logging, Elasticsearch] = ZLayer.fromServiceM { appConfiguration: AppConfigurationService =>
    for {
      config <- appConfiguration.getElasticsearchConfig
      logger <- Logging.getLoggerForClass(this.getClass)
      _ <- logger.info("Elasticsearch bootup started")
      _ <- PortKiller.run(config.port)
      ees <- ZIO.effect { //ZIO.fromFuture { implicit ec =>
        // FIXME: We should be using blocking.effectBlocking() but the Blocking type is causing issues building the layer cake.
        //scala.concurrent.Future {
          EmbeddedElastic.builder()
            .withElasticVersion(config.esVersion)
            .withSetting(PopularProperties.HTTP_PORT, config.port)
            .withSetting(PopularProperties.CLUSTER_NAME, config.clusterName)
            .withCleanInstallationDirectoryOnStop(config.wipeDataOnStartup)
            .build()
            .start()
        //}
      }
      esc <- ZIO.effect {
        ElasticClient(ElasticProperties(
          endpoints = List(ElasticNodeEndpoint(
            protocol = "http",
            host = "localhost",
            port = config.port,
            prefix = None
          )),
          options = Map.empty
        ))
      }
      _ <- logger.info("Elasticsearch bootup finished")
    } yield ElasticsearchServiceLive(ees, esc)
  }

}
