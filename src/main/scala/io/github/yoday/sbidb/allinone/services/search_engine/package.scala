package io.github.yoday.sbidb.allinone.services

import com.sksamuel.elastic4s.searches.sort.SortOrder
import io.github.yoday.sbidb.allinone.Utilities.{fuzzyEquals, isFuzzyListPrefix}
import io.github.yoday.sbidb.allinone.models._
import io.github.yoday.sbidb.allinone.services.elasticsearch.{ESSearchRequest, ESSearchResponse, Elasticsearch, ElasticsearchService, dsl}
import io.github.yoday.sbidb.allinone.services.logging.{Logging, LoggingService}
import io.github.yoday.sbidb.allinone.services.schema_builder.{ESField, SearchEntity}
import zio.json._
import zio.json.ast.Json
import zio.json.ast.Json.{Arr, Num, Obj, Str}
import zio.json.internal.Write
import zio.{Chunk, Function1ToLayerSyntax, Function2ToLayerSyntax, Has, RIO, RLayer, Task, ZIO, ZLayer}

import scala.annotation.tailrec

package object search_engine {

  case class SearchQuery(concepts: List[QueryConcept])

  // TODO: This should use the auto-derivation encoder.
  implicit val searchQueryJsonEncoder: JsonEncoder[SearchQuery] = new JsonEncoder[SearchQuery] {
    override def unsafeEncode(searchQuery: SearchQuery, indent: Option[Int], out: Write): Unit = {
      def toJsonObject(elems: (String, Json)*): Json = {
        Obj(Chunk.fromIterable(elems))
      }

      val json = toJsonObject(
        "concepts" -> Arr(Chunk.fromIterable(searchQuery.concepts.map {
          case MatchExactnessConcept(field, value) =>
            toJsonObject(
              "type" -> Str("MatchExactnessConcept"),
              "field" -> Str(field.fieldName),
              "value" -> Str(value),
              "weight" -> Num(field.weight),
            )
          case NegatedExactnessConcept(field, value) =>
            toJsonObject(
              "type" -> Str("NegatedExactnessConcept"),
              "field" -> Str(field.fieldName),
              "value" -> Str(value),
              "weight" -> Num(field.weight),
            )
          case FuzzyExactnessConcept(field, value) =>
            toJsonObject(
              "type" -> Str("FuzzyExactnessConcept"),
              "field" -> Str(field.fieldName),
              "value" -> Str(value),
              "weight" -> Num(field.weight),
            )
          case IndividualTermConcept(value) =>
            toJsonObject(
              "type" -> Str("IndividualTermConcept"),
              "field" -> Str(SearchEntity.document.fieldName),
              "value" -> Str(value),
              "weight" -> Num(SearchEntity.document.weight),
            )
        }))
      )

      implicitly[JsonEncoder[Json]].unsafeEncode(json, indent, out);
    }
  }

  sealed trait QueryConcept
  case class MatchExactnessConcept(field: ESField, value: String) extends QueryConcept
  case class NegatedExactnessConcept(field: ESField, value: String) extends QueryConcept
  case class FuzzyExactnessConcept(field: ESField, value: String) extends QueryConcept
  // TODO: Implement this.
//  case class SyntacticConcept(field: ESField, value: String) extends QueryConcept
  // TODO: Implement this.
//  case class ExistenceConcept(field: ESField, value: String) extends QueryConcept
  case class IndividualTermConcept(value: String) extends QueryConcept
  // TODO: Implement this.
//  case class RelationalTermConcept(value: String) extends QueryConcept

  case class QueryParts(parts: List[String]) extends AnyVal
  case class ParsingResult(concepts: List[QueryConcept], remainingParts: QueryParts)

  // When an operation is invoked, it either does not match anything or it processes part of the query
  // and returns the list of concepts extracted from it.
  type ParsingOperation = QueryParts => Option[ParsingResult]

  object ExactnessOperation {
    final val REGEX_FOR_ENTRY_OP_VALUE = "^([^~:!]+)([~:!])(.+)$".r
    final val REGEX_FOR_ENTRY_OP = "^([^~:!]+)([~:!])$".r
    final val REGEX_FOR_OP_VALUE = "^([~:!])(.+)$".r

    case class ExactnessClause(key: String, operator: String, value: String, remainingQueryParts: List[String])
  }

  // An operation where the user explicitly wants to match a value against a known data field.
  // the overall pattern looks like: <key> <operator> <value>
  // where <key> is something that maps to a field
  // and <operator> is ":" for exact match, "!" for negated exact match, and "~" for fuzzy match
  // and <value> is just some text to match
  trait ExactnessOperation extends ParsingOperation {
    def keys: Set[String]
    def field: ESField

    override def apply(query: QueryParts): Option[ParsingResult] = {
      for {
        clause <- maybeExtractClause(query.parts)
        concept <- clause.operator match {
          case ":" => Some(MatchExactnessConcept(field, clause.value))
          case "!" => Some(NegatedExactnessConcept(field, clause.value))
          case "~" => Some(FuzzyExactnessConcept(field, clause.value))
          case _ => None
        }
        if keys.contains(clause.key)
      } yield ParsingResult(List(concept), QueryParts(clause.remainingQueryParts))
    }

    // There are 4 permutations of the search clause that can be spread out over 1, 2, or 3 query parts.
    // This function helps break down the query parts into something more manageable.
    // "<key><operator><value>"
    // "<key><operator>" "<value>"
    // "<key>" "<operator><value>"
    // "<key>" "<operator>" "<value>"
    import ExactnessOperation._
    def maybeExtractClause(parts: List[String]): Option[ExactnessClause] = {
      parts match {
        case Nil =>
          None
        case part1 :: part2 :: part3 :: rest =>
          part1 match {
            case REGEX_FOR_ENTRY_OP_VALUE(key, operator, value) =>
              // we were able to extract everything out of part1
              Some(ExactnessClause(key, operator, value, part2 :: part3 :: rest))
            case REGEX_FOR_ENTRY_OP(key, operator) =>
              // we were able to extract the <key> and <operator>, we can assume that the <value> must be in part2
              Some(ExactnessClause(key, operator, part2, part3 :: rest))
            case _ =>
              // we were not able to find an <operator> in part1, lets check part2
              part2 match {
                case REGEX_FOR_OP_VALUE(operator, value) =>
                  // we were able to extract the <operator> and <value> from part2, and we can assume that <key> is in part1
                  Some(ExactnessClause(part1, operator, value, part3 :: rest))
                case _ =>
                  // this case means that part1 will have the <key>, and we can assume that part2 has the <operator>, and part3 has the <value>
                  Some(ExactnessClause(part1, part2, part3, rest))
              }
          }
        case part1 :: part2 :: Nil =>
          part1 match {
            case REGEX_FOR_ENTRY_OP_VALUE(key, operator, value) =>
              // we were able to extract everything out of part1
              Some(ExactnessClause(key, operator, value, part2 :: Nil))
            case REGEX_FOR_ENTRY_OP(key, operator) =>
              // we were able to extract the <key> and <operator>, we can assume that the <value> must be in part2
              Some(ExactnessClause(key, operator, part2, Nil))
            case _ =>
              // we were not able to find an <operator> in part1, lets check part2
              part2 match {
                case REGEX_FOR_OP_VALUE(operator, value) =>
                  // we were able to extract the <operator> and <value> from part2, and we can assume that <key> is in part1
                  Some(ExactnessClause(part1, operator, value, Nil))
                case _ =>
                  // no <operator> found in part1 nor part2 (and no more query parts to look at), so there is nothing for us to do here
                  None
              }
          }
        case part1 :: Nil =>
          part1 match {
            case REGEX_FOR_ENTRY_OP_VALUE(key, operator, value) =>
              // we were able to extract everything out of part1
              Some(ExactnessClause(key, operator, value, Nil))
            case _ =>
              // there is nothing else we can do because there are no more query parts to look at
              None
          }
      }
    }
  }

  // An operation where the user's input follows a pattern that we can match on.
  // This is different from an ExactnessOperation because it's more of a natural-language/fuzzy pattern
  // instead of a fixed pattern.
  trait SyntacticOperation extends ParsingOperation {
  }

  // An operation where the user's input contains some phrases that we can extract some intent from
  // and use use such knowledge to skew results towards better matches.
  trait SemanticOperation extends ParsingOperation {
  }

  // An operation where the user's input matches a small, known set of unique values,
  // that can use such knowledge to skew results towards better matches.
  trait ExistenceOperation extends ParsingOperation {
  }

  case object PersonExactnessOperation extends ExactnessOperation {
    override def field = SearchEntity.person
    override def keys = Set(
      "name",
      "person",
      "people",
      "player",
      "team",
      "game",
      "superbowl",
      "bowl",
      "coach",
      "gm",
      "owner",
    )
  }

  case object LocationExactnessOperation extends ExactnessOperation {
    override def field = SearchEntity.location
    override def keys = Set(
      "loc",
      "location",
      "venue",
      "city",
      "state",
      "province",
      "stadium",
      "field",
    )
  }

  case object DocumentExactnessOperation extends ExactnessOperation {
    override def field = SearchEntity.document
    override def keys = Set(
      "doc",
      "document",
      "data",
      "entity",
    )
  }

  final val EXACTNESS_OPERATIONS = List(
    PersonExactnessOperation,
    LocationExactnessOperation,
    DocumentExactnessOperation,
  )



  object SearchEngine {

    def search(request: SearchRequest): RIO[SearchEngine, Option[SearchResponse]] = ZIO.accessM(_.get.search(request))

  }

  type SearchEngine = Has[SearchEngineService]

  trait SearchEngineService {

    def search(request: SearchRequest): Task[Option[SearchResponse]]

  }

  case class SearchEngineServiceLive(logging: LoggingService, es: ElasticsearchService) extends SearchEngineService {
    import dsl._

    // TODO: Expand this function to support the other operations.
    protected def parseQueryParts(query: QueryParts): ParsingResult = {
      query.parts match {
        case Nil =>
          ParsingResult(
            concepts = List.empty,
            remainingParts = query
          )
        case head :: tail =>
          EXACTNESS_OPERATIONS.foldLeft(Option.empty[ParsingResult]) {
            case (maybeResult, operation) =>
              maybeResult.orElse(operation(query))
          }.getOrElse {
            ParsingResult(
              concepts = List(IndividualTermConcept(head)),
              remainingParts = QueryParts(tail)
            )
          }
      }
    }

    protected def parseQueryText(queryText: String): SearchQuery = {
      val parts = queryText.trim.toLowerCase.split("\\s+").filter(_.nonEmpty).toList

      @tailrec
      def parseQuery(concepts: List[QueryConcept], query: QueryParts): List[QueryConcept] = {
        if (query.parts.isEmpty) {
          concepts
        } else {
          val result = parseQueryParts(query)
          parseQuery(result.concepts ::: concepts, result.remainingParts)
        }
      }

      SearchQuery(parseQuery(List.empty, QueryParts(parts)))
    }

    // TODO: Need to improve the score weights calculation.
    protected def maybeBuildESSearchRequest(searchQuery: SearchQuery): Option[ESSearchRequest] = {
      if (searchQuery.concepts.isEmpty) {
        None
      } else {
        val numTerms = searchQuery.concepts.count {
          case _: IndividualTermConcept => true
          case _ => false
        }
        val individualMultiplier = 0.333D * numTerms / searchQuery.concepts.size
        val exactnessMultiplier = 0.666D * (searchQuery.concepts.size - numTerms) / searchQuery.concepts.size
        Some(
          dsl.search(SearchEntity.index.index)
            .storedFields(
              SearchEntity.docId.fieldName,
              SearchEntity.entityType.fieldName,
              SearchEntity.entityId.fieldName,
            )
            .query(
              boolQuery()
                .should(
                  searchQuery.concepts.zipWithIndex.map {
                    case (MatchExactnessConcept(field, value), i) =>
                      constantScoreQuery(
                        boolQuery()
                          .must(termQuery(field.fieldName, value))
                      )
                        .boost(exactnessMultiplier * field.weight)
                        .queryName(s"_${i}_MatchExactnessConcept")
                    case (NegatedExactnessConcept(field, value), i) =>
                      constantScoreQuery(
                        boolQuery()
                          .not(termQuery(field.fieldName, value))
                      )
                        .boost(exactnessMultiplier * field.weight)
                        .queryName(s"_${i}_NegatedExactnessConcept")
                    case (FuzzyExactnessConcept(field, value), i) =>
                      constantScoreQuery(
                        boolQuery()
                          .not(matchQuery(field.fieldName, value))
                      )
                        .boost(exactnessMultiplier * field.weight * 0.9)
                        .queryName(s"_${i}_FuzzyExactnessConcept")
                    case (IndividualTermConcept(value), i) =>
                      constantScoreQuery(
                        boolQuery()
                          .must(termQuery(SearchEntity.document.fieldName, value))
                      )
                        .boost(individualMultiplier * SearchEntity.document.weight)
                        .queryName(s"_${i}_IndividualTermConcept")
                  }
                )
                .minimumShouldMatch(1)
            )
            .sortBy(scoreSort().order(SortOrder.Desc))
        )
      }
    }

    protected def processESSearchResponse(queryText: String, searchQueryText: String, esQueryText: String, esResponse: ESSearchResponse): Task[SearchResponse] = ZIO.effect {
      SearchResponse(
        queryText = queryText,
        searchQuery = searchQueryText,
        esQuery = esQueryText,
        total = esResponse.totalHits,
        maxScore = esResponse.maxScore,
        results = esResponse.hits.hits.map { hit =>
          SearchResult(
            docId = hit.storedField(SearchEntity.docId.fieldName).value.toString,
            entityType = hit.storedField(SearchEntity.entityType.fieldName).value.toString,
            entityId = hit.storedField(SearchEntity.entityId.fieldName).value.toString,
            score = hit.score.toDouble,
            clauses = hit.matchedQueries.map(_.toList).getOrElse(List.empty)
          )
        }.toList
      )
    }

    override def search(request: SearchRequest): Task[Option[SearchResponse]] = {
      for {
        logger <- logging.getLoggerForClass(this.getClass)
        _ <- logger.debug(s"search: queryText is |${request.queryText}|")
        searchQuery <- ZIO.succeed {
          parseQueryText(request.queryText)
        }
        _ <- logger.debug(s"search: searchQuery json text is ${getSearchQueryJsonText(searchQuery)}")
        result <- if (searchQuery.concepts.isEmpty) {
            ZIO.none
        } else {
          for {
            esRequest <- ZIO.effect {
              maybeBuildESSearchRequest(searchQuery).get
            }
            searchQueryText <- ZIO.effect(getSearchQueryJsonText(searchQuery))
            esQueryText <- es.showRequest(esRequest)
            _ <- logger.debug(s"search: esQueryText is $esQueryText")
            esResponse <- es.executeSearch(esRequest)
            response <- processESSearchResponse(request.queryText, searchQueryText, esQueryText, esResponse)
          } yield Some(response)
        }
      } yield result
    }

    private def getSearchQueryJsonText(searchQuery: SearchQuery): String = {
      implicitly[JsonEncoder[SearchQuery]].encodeJson(searchQuery, Some(0)).toString
    }

  }

  val LIVE: RLayer[Logging with Elasticsearch, SearchEngine] = (SearchEngineServiceLive(_,_)).toLayer

}
