package io.github.yoday.sbidb.allinone.views

import io.github.yoday.sbidb.allinone.Utilities
import io.github.yoday.sbidb.allinone.models._
import io.github.yoday.sbidb.allinone.repositories.player_repo.PlayerRepo
import io.github.yoday.sbidb.allinone.services.mustache.{Mustache, MustacheService}
import zhttp.html._
import zhttp.http._
import zio.{Function1ToLayerSyntax, Has, RIO, RLayer, Task, UIO, ZIO, ZLayer}

package object player_view {

  val ROUTES = Http.collectZIO[Request] {
    case req @ Method.GET -> !! / "view" / "players" / int(playerId) =>
      for {
        maybePlayer <- PlayerRepo.findPlayer(PlayerId(playerId))
        result <- maybePlayer.fold[RIO[PlayerView, Response]] {
          ZIO.succeed {
            Response.fromHttpError(HttpError.NotFound(req.path))
          }
        } { player =>
          PlayerView.renderPlayerPage(player).map { page =>
            Response.html(page)
          }
        }
      } yield result

    case Method.GET -> !! / "view" / "players" =>
      for {
        players <- PlayerRepo.getAllPlayers
        page <- PlayerView.renderAllPlayersPage(players)
      } yield Response.html(page)
  }

  object PlayerView {

    def renderPlayerPage(player: Player): RIO[PlayerView, Html] = ZIO.accessM(_.get.renderPlayerPage(player))

    def renderAllPlayersPage(players: List[Player]): RIO[PlayerView, Html] = ZIO.accessM(_.get.renderAllPlayersPage(players))

  }

  type PlayerView = Has[PlayerViewService]

  trait PlayerViewService {

    def renderPlayerPage(player: Player): Task[Html]

    def renderAllPlayersPage(players: List[Player]): Task[Html]

  }

  case class PlayerViewServiceLive(mustache: MustacheService) extends PlayerViewService {

    override def renderPlayerPage(player: Player): Task[Html] = {
      for {
        template <- mustache.makeTemplate[Player]("player.view.mustache", PLAYER_PAGE_TEMPLATE)
        page <- template.render(player)
      } yield Html.fromString(page)
    }

    override def renderAllPlayersPage(players: List[Player]): Task[Html] = {
      for {
        template <- mustache.makeTemplate[Players]("players.view.mustache", ALL_PLAYERS_PAGE_TEMPLATE)
        page <- template.render(Players(players))
      } yield Html.fromString(page)
    }

  }

  val LIVE: RLayer[Mustache, PlayerView] = (PlayerViewServiceLive(_)).toLayer

  final val PLAYER_INFO_CARD = """
    <div class="container">
      <div class="row">
        <div class="col-3" align="right">id:</div>
        <div class="col-9">{{id}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">firstName:</div>
        <div class="col-9">{{firstName}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">lastName:</div>
        <div class="col-9">{{lastName}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">position:</div>
        <div class="col-9">{{position}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">birthDate:</div>
        <div class="col-9">{{birthDate}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">birthCity:</div>
        <div class="col-9">{{birthCity}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">birthState:</div>
        <div class="col-9">{{birthState}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">college:</div>
        <div class="col-9">{{college}}</div>
      </div>
    </div>
  """

  final val PLAYER_PAGE_TEMPLATE = Utilities.makeBootstrapPage(
    innerHead = s"""
      <title>SBIDB Player {{id}}</title>
    """,
    innerBody = s"""
      $PLAYER_INFO_CARD
    """
  )

  final val ALL_PLAYERS_PAGE_TEMPLATE = Utilities.makeBootstrapPage(
    innerHead = s"""
      <title>SBIDB All Players</title>
    """,
    innerBody = s"""
      <div class="container">
        {{#players}}
          <div class="row">
            $PLAYER_INFO_CARD
            <hr />
          </div>
        {{/players}}
      </div>
    """
  )

}
