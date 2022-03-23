package io.github.yoday.sbidb.allinone.views

import io.github.yoday.sbidb.allinone.services.elasticsearch.Elasticsearch
import io.github.yoday.sbidb.allinone.services.schema_builder.SearchEntity
import io.github.yoday.sbidb.allinone.services.slick._
import io.github.yoday.sbidb.allinone.models._
import zhttp.http._
import zio.{Has, RIO, UIO, ZIO}

package object health_checks {

  val ROUTES = Http.collectZIO[Request] {
    case Method.GET -> !! / "health_checks" / "http_server" =>
      UIO(Response.text("http_server: OK"))

    case Method.GET -> !! / "health_checks" / "postgres" =>
      Slick.getPostgresVersion.map { version =>
        Response.text(s"postgres: $version")
      }

    case Method.GET -> !! / "health_checks" / "elasticsearch" =>
      Elasticsearch.getClusterVersion.map { version =>
        Response.text(s"elasticsearch: $version")
      }

    // TODO: Move this to it's own view and service.
    case req @ Method.GET -> !! / "es" / docId =>
      Elasticsearch.getDocument(SearchEntity, docId).map {
          case None =>
            Response.fromHttpError(HttpError.NotFound(req.path))
          case Some(doc) =>
            Response.json(jsonJsonCodec.encodeJson(doc, Some(0)).toString)
      }
  }

}
