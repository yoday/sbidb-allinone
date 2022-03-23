package io.github.yoday.sbidb.allinone.views

import io.github.yoday.sbidb.allinone.Utilities
import io.github.yoday.sbidb.allinone.models._
import io.github.yoday.sbidb.allinone.repositories.team_repo.TeamRepo
import io.github.yoday.sbidb.allinone.services.mustache.{Mustache, MustacheService}
import zhttp.html._
import zhttp.http._
import zio.{Function1ToLayerSyntax, Has, RIO, RLayer, Task, UIO, ZIO, ZLayer}

package object team_view {

  val ROUTES = Http.collectZIO[Request] {
    case req @ Method.GET -> !! / "view" / "teams" / int(teamId) =>
      for {
        maybeTeam <- TeamRepo.findTeam(TeamId(teamId))
        result <- maybeTeam.fold[RIO[TeamView, Response]] {
          ZIO.succeed {
            Response.fromHttpError(HttpError.NotFound(req.path))
          }
        } { team =>
          TeamView.renderTeamPage(team).map { page =>
            Response.html(page)
          }
        }
      } yield result

    case Method.GET -> !! / "view" / "teams" =>
      for {
        teams <- TeamRepo.getAllTeams
        page <- TeamView.renderAllTeamsPage(teams)
      } yield Response.html(page)
  }

  object TeamView {

    def renderTeamPage(team: Team): RIO[TeamView, Html] = ZIO.accessM(_.get.renderTeamPage(team))

    def renderAllTeamsPage(teams: List[Team]): RIO[TeamView, Html] = ZIO.accessM(_.get.renderAllTeamsPage(teams))

  }

  type TeamView = Has[TeamViewService]

  trait TeamViewService {

    def renderTeamPage(team: Team): Task[Html]

    def renderAllTeamsPage(teams: List[Team]): Task[Html]

  }

  case class TeamViewServiceLive(mustache: MustacheService) extends TeamViewService {

    override def renderTeamPage(team: Team): Task[Html] = {
      for {
        template <- mustache.makeTemplate[Team]("team.view.mustache", TEAM_PAGE_TEMPLATE)
        page <- template.render(team)
      } yield Html.fromString(page)
    }

    override def renderAllTeamsPage(teams: List[Team]): Task[Html] = {
      for {
        template <- mustache.makeTemplate[Teams]("teams.view.mustache", ALL_TEAMS_PAGE_TEMPLATE)
        page <- template.render(Teams(teams))
      } yield Html.fromString(page)
    }

  }

  val LIVE: RLayer[Mustache, TeamView] = (TeamViewServiceLive(_)).toLayer

  final val TEAM_INFO_CARD = """
    <div class="container">
      <div class="row">
        <div class="col-3" align="right">id:</div>
        <div class="col-9">{{id}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">mascot:</div>
        <div class="col-9">{{mascot}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">city:</div>
        <div class="col-9">{{city}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">state:</div>
        <div class="col-9">{{state}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">owner:</div>
        <div class="col-9">{{owner}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">gm:</div>
        <div class="col-9">{{gm}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">coach:</div>
        <div class="col-9">{{coach}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">yearFounded:</div>
        <div class="col-9">{{yearFounded}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">stadium:</div>
        <div class="col-9">{{stadium}}</div>
      </div>
      <div class="row">
        <div class="col-3" align="right">division:</div>
        <div class="col-9">{{division}}</div>
      </div>
    </div>
  """


  final val TEAM_PAGE_TEMPLATE = Utilities.makeBootstrapPage(
    innerHead = s"""
      <title>SBIDB Team {{id}}</title>
    """,
    innerBody = s"""
      $TEAM_INFO_CARD
    """
  )

  final val ALL_TEAMS_PAGE_TEMPLATE = Utilities.makeBootstrapPage(
    innerHead = s"""
      <title>SBIDB All Teams</title>
    """,
    innerBody = s"""
      <div class="container">
        {{#teams}}
          <div class="row">
            $TEAM_INFO_CARD
            <hr />
          </div>
        {{/teams}}
      </div>
    """
  )

}
