package io.github.yoday.sbidb.allinone.views

import io.github.yoday.sbidb.allinone.Utilities
import io.github.yoday.sbidb.allinone.models._
import io.github.yoday.sbidb.allinone.repositories.game_repo.GameRepo
import io.github.yoday.sbidb.allinone.services.mustache.{Mustache, MustacheService}
import zhttp.html._
import zhttp.http._
import zio.{Function1ToLayerSyntax, Has, RIO, RLayer, Task, UIO, ZIO, ZLayer}

package object game_view {

  val ROUTES = Http.collectZIO[Request] {
    case req@Method.GET -> !! / "view" / "games" / int(gameId) =>
      for {
        maybeGame <- GameRepo.findGame(GameId(gameId))
        result <- maybeGame.fold[RIO[GameView, Response]] {
          ZIO.succeed {
            Response.fromHttpError(HttpError.NotFound(req.path))
          }
        } { game =>
          GameView.renderGamePage(game).map { page =>
            Response.html(page)
          }
        }
      } yield result

    case Method.GET -> !! / "view" / "games" =>
      for {
        games <- GameRepo.getAllGames
        page <- GameView.renderAllGamesPage(games)
      } yield Response.html(page)
  }

  object GameView {

    def renderGamePage(game: Game): RIO[GameView, Html] = ZIO.accessM(_.get.renderGamePage(game))

    def renderAllGamesPage(games: List[Game]): RIO[GameView, Html] = ZIO.accessM(_.get.renderAllGamesPage(games))

  }

  type GameView = Has[GameViewService]

  trait GameViewService {

    def renderGamePage(game: Game): Task[Html]

    def renderAllGamesPage(games: List[Game]): Task[Html]

  }

  case class GameViewServiceLive(mustache: MustacheService) extends GameViewService {

    override def renderGamePage(game: Game): Task[Html] = {
      for {
        template <- mustache.makeTemplate[Game]("game.view.mustache", GAME_PAGE_TEMPLATE)
        page <- template.render(game)
      } yield Html.fromString(page)
    }

    override def renderAllGamesPage(games: List[Game]): Task[Html] = {
      for {
        template <- mustache.makeTemplate[Games]("games.view.mustache", ALL_GAMES_PAGE_TEMPLATE)
        page <- template.render(Games(games))
      } yield Html.fromString(page)
    }

  }

  val LIVE: RLayer[Mustache, GameView] = (GameViewServiceLive(_)).toLayer

  final val GAME_INFO_CARD = """
    <div class="container">
      <div class="row">
        <div class="col-3" align="right">id:</div>
        <div class="col-9">{{id}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">team1Id:</div>
        <div class="col-9"><a href="/view/teams/{{team1Id}}">{{team1Id}}</a></div>
      </div>
      <div class="row">
        <div class="col-3" align="right">team2Id:</div>
        <div class="col-9"><a href="/view/teams/{{team2Id}}">{{team2Id}}</a></div>
      </div>
      <div class="row">
        <div class="col-3" align="right">mvpPlayerId:</div>
        <div class="col-9"><a href="/view/players/{{mvpPlayerId}}">{{mvpPlayerId}}</a></div>
      </div>
      <div class="row">
        <div class="col-3" align="right">mvpTeamId:</div>
        <div class="col-9"><a href="/view/teams/{{mvpTeamId}}">{{mvpTeamId}}</a></div>
      </div>
      <div class="row">
        <div class="col-3" align="right">team1Score:</div>
        <div class="col-9">{{team1Score}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">team2Score:</div>
        <div class="col-9">{{team2Score}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">venueName:</div>
        <div class="col-9">{{venueName}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">venueCity:</div>
        <div class="col-9">{{venueCity}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">venueState:</div>
        <div class="col-9">{{venueState}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">gameDate:</div>
        <div class="col-9">{{gameDate}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">attendance:</div>
        <div class="col-9">{{attendance}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">gameNumeral:</div>
        <div class="col-9">{{gameNumeral}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">mvpBlurb:</div>
        <div class="col-9"><p>{{mvpBlurb}}</p></div>
      </div>
      <div class="row">
        <div class="col-3" align="right">mvpStats:</div>
        <div class="col-9"><pre>{{mvpStats}}</pre></div>
      </div>
    </div>
  """

  final val GAME_PAGE_TEMPLATE = Utilities.makeBootstrapPage(
    innerHead = s"""
      <title>SBIDB Game {{id}}</title>
    """,
    innerBody = s"""
      $GAME_INFO_CARD
    """
  )

  final val ALL_GAMES_PAGE_TEMPLATE = Utilities.makeBootstrapPage(
    innerHead = s"""
      <title>SBIDB All Games</title>
    """,
    innerBody = s"""
      <div class="container">
        {{#games}}
          <div class="row">
            $GAME_INFO_CARD
            <hr />
          </div>
        {{/games}}
      </div>
    """
  )

}
