package io.github.yoday.sbidb.allinone.services

import com.sksamuel.elastic4s.{Index, IndexAndType}
import io.github.yoday.sbidb.allinone.services.elasticsearch.{Elasticsearch, ElasticsearchService, dsl}
import io.github.yoday.sbidb.allinone.services.logging.{Logging, LoggingService}
import zio.{Chunk, ChunkBuilder, Function2ToLayerSyntax, Has, RIO, RLayer, Task, ULayer, ZIO, ZLayer}
import zio.json.{JsonCodec, JsonEncoder}
import zio.json.ast.Json
import zio.json.ast.Json.{Arr, Obj, Str}

package object schema_builder {

  object SearchEntity extends ESDocument {
    override def index = IndexAndType("search_entity", "search_entity")

    object docId      extends TermField(1.000, "docId")
    object entityType extends TermField(1.000, "entityType")
    object entityId   extends TermField(1.000, "entityId")
    object person     extends SoupField(0.900, "person")
    object location   extends SoupField(0.850, "location")
    object document   extends SoupField(0.800, "document")

    override def fields = List(
      docId,
      entityType,
      entityId,
      person,
      location,
      document,
    )

    override def idField = docId
  }

  trait ESDocument {
    def index: IndexAndType

    def fields: List[ESField]

    def idField: ESField

    def findFieldByBaseName(baseName: String): Option[ESField] = {
      fields.find(_.baseName == baseName)
    }
    def findFieldByFieldName(fieldName: String): Option[ESField] = {
      fields.find(_.fieldName == fieldName)
    }
  }

  sealed trait ESField {
    // The full name of the field as it is stored in ElasticSearch.
    def fieldName: String = baseName + suffix

    // The general name we use for the data.
    def baseName: String

    // A string used to categorize fields with similar functionality, setup, and/or usage.
    def suffix: String

    // A base value used to adjust the relevance of this field compared to other fields in the document.
    // In short, some fields are more important than others. The bigger the weight, the more important.
    def weight: Double
  }

  // These types of fields are generally used for exact matches using term queries/filters.
  case class TermField(weight: Double, baseName: String) extends ESField {
    override def suffix: String = "_term"
  }

  // These types of fields have an amalgamation of values.
  case class SoupField(weight: Double, baseName: String) extends ESField {
    override def suffix: String = "_soup"
  }

  object SchemaBuilder {

  }

  type SchemaBuilder = Has[SchemaBuilderService]

  trait SchemaBuilderService {

    def setupIndex(document: ESDocument): Task[Unit]

    def indexDocument[T : JsonEncoder](document: ESDocument, data: T): Task[Unit]

  }

  case class SchemaBuilderServiceLive(logging: LoggingService, es: ElasticsearchService) extends SchemaBuilderService {
    import dsl._

    override def setupIndex(document: ESDocument): Task[Unit] = {
      for {
        req <- ZIO.succeed {
          createIndex(document.index.index)
            .mappings(mapping(document.index.`type`)
              .all(false)
              .source(true)
              //.size(true)
              .fields(document.fields.map {
                case f: TermField =>
                  keywordField(f.fieldName)
                    .stored(true)
                case f: SoupField =>
                  //textField(f.fieldName)
                  keywordField(f.fieldName)
                    .stored(true)
              })
            )
        }
        logger <- logging.getLoggerForClass(this.getClass)
        requestText <- es.showRequest(req)
        _ <- logger.debug(s"setupIndex request text: $requestText")
        _ <- es.setupIndex(req)
      } yield ()
    }

    override def indexDocument[T : JsonEncoder](document: ESDocument, data: T): Task[Unit] = {
      for {
        initialJson <- ZIO.fromEither {
          implicitly[JsonEncoder[T]].toJsonAST(data).left.map { err =>
            new RuntimeException(s"Failed to convert data object into json for ES document=${document.index} error=$err")
          }
        }
        esJson <- ZIO.fromEither {
          (initialJson match {
            case Obj(jsonFields) =>
              eitherConvertToESJsonOrError(document, jsonFields)
            case _ =>
              Left("Expected object structure in json")
          }).left.map { err =>
            new RuntimeException(s"Failed to convert data object into json for ES document=${document.index} error=$err")
          }
        }
        docId <- ZIO.fromEither {
          esJson.collectFirst {
            case (name, Str(value)) if document.idField.fieldName == name =>
              value
          }.toRight {
            new RuntimeException(s"Failed to find idField '${document.idField.fieldName}' in json for indexing into ES.")
          }
        }
        req <- ZIO.succeed {
          indexInto(document.index)
            .id(docId)
            .source(Json.encoder.encodeJson(Obj(esJson), None).toString)
        }
        _ <- es.indexDocument(req)
      } yield ()
    }

    private type JsonField = (String, Json)
    protected def eitherConvertToESJsonOrError(document: ESDocument, jsonFields: Chunk[JsonField]): Either[String, Chunk[JsonField]] = {
      jsonFields.foldLeft[Either[String, ChunkBuilder[JsonField]]](Right(ChunkBuilder.make[JsonField]())) {
        case (Right(chunkBuilder), (name, json)) =>
          document.findFieldByBaseName(name) match {
            case None =>
              Left(s"Unknown field '$name' was found in indexing json for ES document=${document.index}")
            case Some(field) =>
              Right(chunkBuilder += field.fieldName -> json)
          }
        case (Left(err), _) =>
          Left(err)
      }.map(_.result())
    }

  }

  val LIVE: RLayer[Logging with Elasticsearch, SchemaBuilder] = (SchemaBuilderServiceLive(_,_)).toLayer

}
