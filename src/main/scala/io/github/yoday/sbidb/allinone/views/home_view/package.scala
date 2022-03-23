package io.github.yoday.sbidb.allinone.views

import io.github.yoday.sbidb.allinone.Utilities
import io.github.yoday.sbidb.allinone.services.mustache.Mustache
import zhttp.html.Html
import zhttp.http.{Http, Method, Request, Response}
import zhttp.http._
import zio.UIO
import zio.json.{DeriveJsonCodec, JsonCodec}

package object home_view {

  val ROUTES = Http.collectZIO[Request] {
    case Method.GET -> !! =>
      UIO(Response.redirect("/home"))

    case Method.GET -> !! / "index" =>
      UIO(Response.redirect("/home"))

    case Method.GET -> !! / "home" =>
      for {
        template <- Mustache.makeTemplate[Pages]("home.view.mustache", HOME_PAGE_TEMPLATE)
        page <- template.render(Pages(List(
          Page("/home", "The main page of the site. (this page)"),
          Page("/health_checks/http_server", "An endpoint to test the health of the server"),
          Page("/health_checks/postgres", "An endpoint to test the server's connectivity to the PostgreSQL database."),
          Page("/health_checks/elasticsearch", "An endpoint to test the server's connectivity to the ElasticSearch cluster."),
          Page("/view/players", "The primary page to see the data for all known players."),
          Page("/view/teams", "The primary page to see the data for all known teams."),
          Page("/view/games", "The primary page to see the data for all known games."),
        )))
      } yield Response.html(Html.fromString(page))

  }

  // Just a couple of helper classes to make the page easier to render using the template system.
  private case class Page(link: String, description: String)
  private case class Pages(pages: List[Page])
  private implicit val pageJsonCodec: JsonCodec[Page] = DeriveJsonCodec.gen[Page]
  private implicit val pagesJsonCodec: JsonCodec[Pages] = DeriveJsonCodec.gen[Pages]

  final val HOME_PAGE_TEMPLATE = Utilities.makeBootstrapPage(
    innerHead = s"""
      <title>SBIDB Home</title>
    """,
    innerBody = s"""
      <div class="container">
        {{#pages}}
          <div class="row">
            <div class="col-3"><a href="{{link}}">{{link}}</a></div>
            <div class="col-9">{{description}}</div>
          </div>
        {{/pages}}
      </div>
    """
  )

}
