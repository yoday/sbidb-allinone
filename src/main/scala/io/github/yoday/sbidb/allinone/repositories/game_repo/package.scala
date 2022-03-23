package io.github.yoday.sbidb.allinone.repositories

import io.github.yoday.sbidb.allinone.models._
import io.github.yoday.sbidb.allinone.services.slick.{Slick, SlickService}
import zio.{Function1ToLayerSyntax, Has, RIO, Task, URLayer, ZIO, ZLayer}
import io.github.yoday.sbidb.allinone.services.slick.profile.api._
import zio.json.ast.Json

import java.time.LocalDate

package object game_repo {

  class GamesTable(tag: Tag) extends Table[Game](tag, "games") {
    def id = column[GameId]("id", O.PrimaryKey)
    def team1Id = column[TeamId]("team1_id")
    def team2Id = column[TeamId]("team2_id")
    def mvpPlayerId = column[PlayerId]("mvp_player_id")
    def mvpTeamId = column[TeamId]("mvp_team_id")
    def team1Score = column[Int]("team1_score")
    def team2Score = column[Int]("team2_score")
    def venueName = column[String]("venue_name")
    def venueCity = column[String]("venue_city")
    def venueState = column[String]("venue_state")
    def gameDate = column[LocalDate]("game_date")
    def attendance = column[Int]("attendance")
    def gameNumeral = column[String]("game_numeral")
    def mvpBlurb = column[String]("mvp_blurb")
    def mvpStats = column[Json]("mvp_stats")

    def * = (
      id,
      team1Id,
      team2Id,
      mvpPlayerId,
      mvpTeamId,
      team1Score,
      team2Score,
      venueName,
      venueCity,
      venueState,
      gameDate,
      attendance,
      gameNumeral,
      mvpBlurb,
      mvpStats
    ) <> (Game.tupled, Game.unapply)
  }

  object GameRepo {

    def findGame(id: GameId): RIO[GameRepo, Option[Game]] = ZIO.accessM(_.get.findGame(id))

    def getAllGames: RIO[GameRepo, List[Game]] = ZIO.accessM(_.get.getAllGames)

    def getAllGamesForPlayer(id: PlayerId): RIO[GameRepo, List[Game]] = ZIO.accessM(_.get.getAllGamesForPlayer(id))

    def getAllGamesForTeam(id: TeamId): RIO[GameRepo, List[Game]] = ZIO.accessM(_.get.getAllGamesForTeam(id))

  }

  type GameRepo = Has[GameRepoService]

  trait GameRepoService {

    def findGame(id: GameId): Task[Option[Game]]

    def getAllGames: Task[List[Game]]

    def getAllGamesForPlayer(id: PlayerId): Task[List[Game]]

    def getAllGamesForTeam(id: TeamId): Task[List[Game]]

  }

  case class GameRepoServiceLive(slick: SlickService) extends GameRepoService {

    override def findGame(id: GameId): Task[Option[Game]] = {
      for {
        table <- ZIO.effect(new TableQuery(tag => new GamesTable(tag)))
        query <- ZIO.succeed(table.filter(_.id === id).result)
        result <- slick.execute(query.headOption)
      } yield result
    }

    override def getAllGames: Task[List[Game]] = {
      for {
        table <- ZIO.effect(new TableQuery(tag => new GamesTable(tag)))
        query <- ZIO.succeed(table.result)
        result <- slick.execute(query).map(_.toList)
      } yield result
    }

    override def getAllGamesForPlayer(id: PlayerId): Task[List[Game]] = {
      for {
        table <- ZIO.effect(new TableQuery(tag => new GamesTable(tag)))
        query <- ZIO.succeed(table.filter(_.mvpPlayerId === id).result)
        result <- slick.execute(query).map(_.toList)
      } yield result
    }

    override def getAllGamesForTeam(id: TeamId): Task[List[Game]] = {
      for {
        table <- ZIO.effect(new TableQuery(tag => new GamesTable(tag)))
        query <- ZIO.succeed(table.filter(x => x.team1Id === id || x.team2Id === id ).result)
        result <- slick.execute(query).map(_.toList)
      } yield result
    }

  }

  val LIVE: URLayer[Slick, GameRepo] = (GameRepoServiceLive(_)).toLayer

}
