package io.github.yoday.sbidb.allinone.views

import io.github.yoday.sbidb.allinone.Utilities
import io.github.yoday.sbidb.allinone.models._
import io.github.yoday.sbidb.allinone.services.logging.{Logging, LoggingService}
import io.github.yoday.sbidb.allinone.services.mustache.{Mustache, MustacheService}
import io.github.yoday.sbidb.allinone.services.search_engine.{SearchEngine, SearchEngineService}
import zhttp.html.Html
import zhttp.http.{Http, Method, Request, Response}
import zhttp.http._
import zio.{Function2ToLayerSyntax, Function3ToLayerSyntax, Has, RIO, RLayer, Task, UIO, ZIO, ZLayer}

package object search_result_view {

  val ROUTES = Http.collectZIO[Request] {
    case req @ Method.GET -> !! / "search" =>
      for {
        searchText <- ZIO.succeed {
          val param = req.url.queryParams.get("q")
          param.fold("") {
            _.map(_.trim).filter(_.nonEmpty).mkString(" ")
          }
        }
        page <- SearchResultView.renderSearchResults(searchText)
      } yield Response.html(page)
  }

  object SearchResultView {

    def renderSearchResults(searchText: String): RIO[SearchResultView, Html] = ZIO.accessM(_.get.renderSearchResults(searchText))

  }

  type SearchResultView = Has[SearchResultViewService]

  trait SearchResultViewService {

    def renderSearchResults(searchText: String): Task[Html]

  }

  case class SearchResultViewServiceLive(logging: LoggingService, mustache: MustacheService, searchEngine: SearchEngineService) extends SearchResultViewService {

    override def renderSearchResults(searchText: String): Task[Html] = {
      for {
        maybeResponse <- searchEngine.search(SearchRequest(searchText))
        result <- maybeResponse match {
          case None =>
            UIO(NO_SEARCH_RESULT_PAGE_TEMPLATE)
          case Some(response) =>
            for {
              template <- mustache.makeTemplate[SearchResponse]("search_result.view.mustache", SEARCH_RESULT_PAGE_TEMPLATE)
              page <- template.render(response)
            } yield page
        }
      } yield Html.fromString(result)
    }

  }

  val LIVE: RLayer[Logging with Mustache with SearchEngine, SearchResultView] = (SearchResultViewServiceLive(_,_,_)).toLayer

  final val SEARCH_RESULT_PAGE_TEMPLATE = Utilities.makeBootstrapPage(
    innerHead = s"""
      <title>SBIDB Search Results</title>
    """,
    innerBody = s"""
      <div class="container">
        <div class="row">
          <div class="col-3" align="right">queryText:</div>
          <div class="col-9">{{queryText}}</div>
        </div>
        <div class="row">
          <div class="col-3" align="right">maxScore:</div>
          <div class="col-9">{{maxScore}}</div>
        </div>
        <div class="row">
          <div class="col-3" align="right">total:</div>
          <div class="col-9">{{total}}</div>
        </div>
        <div class="row">
          <div class="col-3" align="right">searchQuery:</div>
          <div class="col-9"><pre>{{searchQuery}}</pre></div>
        </div>
        <div class="row">
          <div class="col-3" align="right">esQuery:</div>
          <div class="col-9"><pre>{{esQuery}}</pre></div>
        </div>
        <hr />

        {{#results}}
          <div class="row">

            <div class="container">
              <div class="row">
                <div class="col-3" align="right">[view] docId:</div>
                <div class="col-9"><a href="/view/{{entityType}}s/{{entityId}}">{{docId}}</a></div>
              </div>
              <div class="row">
                <div class="col-3" align="right">[es] docId:</div>
                <div class="col-9"><a href="/es/{{docId}}">{{docId}}</a></div>
              </div>
              <div class="row">
                <div class="col-3" align="right">entityType:</div>
                <div class="col-9">{{entityType}}</div>
              </div>
              <div class="row">
                <div class="col-3" align="right">entityId:</div>
                <div class="col-9">{{entityId}}</div>
              </div>
              <div class="row">
                <div class="col-3" align="right">score:</div>
                <div class="col-9">{{score}}</div>
              </div>
              <div class="row">
                <div class="col-3" align="right">clauses:</div>
                <div class="col-9">
                  <div class="container">
                    {{#clauses}}
                      <div class="row">
                        <div class="col">{{.}}</div>
                      </div>
                    {{/clauses}}
                  </div>
                </div>
              </div>
            </div>

            <hr />
          </div>
        {{/results}}
      </div>
    """
  )

  final val NO_SEARCH_RESULT_PAGE_TEMPLATE = Utilities.makeBootstrapPage(
    innerHead = s"""
      <title>SBIDB Search Results</title>
    """,
    innerBody = s"""
      <span>No search criteria was provided.</span>
    """
  )

}
