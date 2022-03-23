package io.github.yoday.sbidb.allinone.services

import com.github.tminglei.slickpg.{ExPostgresProfile, PgDate2Support}
import io.github.yoday.sbidb.allinone.services.postgres._
import zio.{Has, RIO, RLayer, Task, ZIO, ZLayer}
import _root_.slick.basic
import _root_.slick.jdbc
import _root_.slick.util
import com.github.tminglei.slickpg.json.PgJsonExtensions
import com.github.tminglei.slickpg.utils.PlainSQLUtils._
import zio.json.ast.Json

import scala.concurrent.duration.DurationInt
import scala.reflect.classTag

// TODO: Look into using zio-slick-interop library.
package object slick {

  type SlickIO[+R] = profile.api.DBIO[R]
  type SlickDatabase = profile.backend.Database

  object profile extends SlickProfile
  trait SlickProfile extends ExPostgresProfile
    with PgDate2Support
    with PgJsonExtensions
  {
    def pgjson = "jsonb"

    // Add back `capabilities.insertOrUpdate` to enable native `upsert` support; for postgres 9.5+
    override protected def computeCapabilities: Set[basic.Capability] =
      super.computeCapabilities + jdbc.JdbcCapabilities.insertOrUpdate

    object SlickAPI extends API
      with DateTimeImplicits
    {

      // register types to let `ExModelBuilder` find them
      bindPgTypeToScala("json", classTag[Json])
      bindPgTypeToScala("jsonb", classTag[Json])

      def parseJsonOrFail(str: String): Json = {
        Json.decoder.decodeJson(str) match {
          case Left(err) => throw new RuntimeException(s"Failed to parse database string into json: $err")
          case Right(value) => value
        }
      }

      def jsonToString(json: Json): String = {
        Json.encoder.encodeJson(json, None).toString
      }

      implicit val zioJsonTypeMapper: jdbc.JdbcType[Json] =
        new GenericJdbcType[Json](pgjson, parseJsonOrFail, jsonToString, hasLiteralForm = false)

//      implicit def zioJsonColumnExtensionMethods(c: Rep[Json]) = {
//        JsonColumnExtensionMethods[Json, Json](c)
//      }
//      implicit def zioJsonOptionColumnExtensionMethods(c: Rep[Option[Json]]) = {
//        JsonColumnExtensionMethods[Json, Option[Json]](c)
//      }

      implicit class PgJsonPositionedResult(r: jdbc.PositionedResult) {
        def nextJson(): Json = nextJsonOption().getOrElse(Json.Null)
        def nextJsonOption(): Option[Json] = r.nextStringOption().map(parseJsonOrFail)
      }

      implicit val getJson = mkGetResult(_.nextJson())
      implicit val getJsonOption = mkGetResult(_.nextJsonOption())
      implicit val setJson = mkSetParameter[Json](pgjson, jsonToString)
      implicit val setJsonOption = mkOptionSetParameter[Json](pgjson, jsonToString)

    }
    override val api = SlickAPI
  }

  object Slick {

    def execute[R](action: SlickIO[R]): RIO[Slick, R] = ZIO.accessM(_.get.execute(action))

    def getPostgresVersion: RIO[Slick, String] = ZIO.accessM(_.get.getPostgresVersion)

  }

  type Slick = Has[SlickService]

  trait SlickService {

    def execute[R](action: SlickIO[R]): Task[R]

    def getPostgresVersion: Task[String]

  }

  case class SlickServiceLive(database: SlickDatabase) extends SlickService {
    import profile.api._

    override def execute[R](action: SlickIO[R]): Task[R] = ZIO.fromFuture { implicit ec =>
      database.run(action)
    }

    override def getPostgresVersion: Task[String] = {
      execute(sql"SELECT version() :: TEXT ;".as[String].head)
    }

  }

  // TODO: Figure out a cleaner way to initialize the service; perhaps using ZManaged.
  val LIVE: RLayer[Postgres, Slick] = ZLayer.fromServiceM { postgres: PostgresService =>
    for {
      jdbcDatasource <- postgres.getDataSource
      slickDatabase <- ZIO.effect {
        profile.api.Database.forDataSource(
          ds = jdbcDatasource,
          maxConnections = Some(1),
          executor = util.AsyncExecutor.apply(
            name = "SlickAsyncExecutor",
            numThreads = 1,
            queueSize = 1,
//            minThreads = 1,
//            maxThreads = 1,
//            queueSize = 0,
//            maxConnections = Integer.MAX_VALUE,
//            keepAliveTime = 5.minute,
//            registerMbeans = true
          ),
          keepAliveConnection = false
        )
      }
    } yield SlickServiceLive(slickDatabase)
  }

}
