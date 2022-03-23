package io.github.yoday.sbidb.allinone.services

import com.github.mustachejava.resolver.ClasspathResolver
import com.github.mustachejava.{DefaultMustacheFactory, MustacheFactory, Mustache => JMustache}
import zio.{Has, RIO, Task, ULayer, ZIO, ZLayer}
import zio.json._
import zio.json.ast.Json
import zio.json.ast.Json._

import java.io.{StringReader, StringWriter}
import java.util

package object mustache {

  trait MustacheTemplate[-T] {
    def render(model: T): Task[String]
  }

  object Mustache {

    def makeTemplate[T : JsonEncoder](name: String, text: String): RIO[Mustache, MustacheTemplate[T]] = ZIO.accessM(_.get.makeTemplate[T](name, text))

  }

  type Mustache = Has[MustacheService]

  trait MustacheService {

    def makeTemplate[T : JsonEncoder](name: String, text: String): Task[MustacheTemplate[T]]

  }

  case class MustacheServiceLive(mf: MustacheFactory) extends MustacheService {

    override def makeTemplate[T](name: String, text: String)(implicit ev: JsonEncoder[T]): Task[MustacheTemplate[T]] = {
      val reader = new StringReader(text)
      val mt = mf.compile(reader, name)

      def jsonToJava(json: Json): Object = json match {
        case Obj(fields) =>
          val hashMap = new util.HashMap[String, Object](fields.size)
          fields.foreach { entry =>
            hashMap.put(entry._1, jsonToJava(entry._2))
          }
          hashMap

        case Arr(elements) =>
          val arrayList = new util.ArrayList[Object](elements.size)
          elements.foreach { entry =>
            arrayList.add(jsonToJava(entry))
          }
          arrayList

        case Bool(value) =>
          value.asInstanceOf[AnyRef]

        case Str(value) =>
          value

        case Num(value) =>
          value.toString

        case Null =>
          null
      }

      ZIO.succeed((model: T) => {
        for {
          json <- ZIO.fromEither {
            ev.toJsonAST(model).left.map { message =>
              new RuntimeException(s"Unexpected error when converting model to json for template '$name'. error: $message")
            }
          }
          result <- ZIO.effect {
            val data = jsonToJava(json)
            val writer = new StringWriter()
            mt.execute(writer, data)
            writer.toString
          }
        } yield result
      })
    }

  }

  val LIVE: ULayer[Mustache] = ZLayer.succeed {
    val mr = new ClasspathResolver()
    val mf = new DefaultMustacheFactory(mr)
    MustacheServiceLive(mf)
  }

}
