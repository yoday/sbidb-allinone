package io.github.yoday.sbidb.allinone.repositories

import io.github.yoday.sbidb.allinone.models._
import io.github.yoday.sbidb.allinone.services.slick.{Slick, SlickService}
import zio.{Function1ToLayerSyntax, Has, RIO, Task, URLayer, ZIO, ZLayer}
import io.github.yoday.sbidb.allinone.services.slick.profile.api._

import java.time.LocalDate

package object player_repo {

  class PlayersTable(tag: Tag) extends Table[Player](tag, "players") {
    def id = column[PlayerId]("id", O.PrimaryKey)
    def firstName = column[String]("first_name")
    def lastName = column[String]("last_name")
    def position = column[String]("position")
    def birthDate = column[LocalDate]("birth_date")
    def birthCity = column[String]("birth_city")
    def birthState = column[String]("birth_state")
    def college = column[String]("college")

    def * = (
      id,
      firstName,
      lastName,
      position,
      birthDate,
      birthCity,
      birthState,
      college
    ) <> (Player.tupled, Player.unapply)
  }

  object PlayerRepo {

    def findPlayer(id: PlayerId): RIO[PlayerRepo, Option[Player]] = ZIO.accessM(_.get.findPlayer(id))

    def getAllPlayers: RIO[PlayerRepo, List[Player]] = ZIO.accessM(_.get.getAllPlayers)

  }

  type PlayerRepo = Has[PlayerRepoService]

  trait PlayerRepoService {

    def findPlayer(id: PlayerId): Task[Option[Player]]

    def getAllPlayers: Task[List[Player]]

  }

  case class PlayerRepoServiceLive(slick: SlickService) extends PlayerRepoService {

    override def findPlayer(id: PlayerId): Task[Option[Player]] = {
      for {
        table <- ZIO.effect(new TableQuery(tag => new PlayersTable(tag)))
        query <- ZIO.succeed(table.filter(_.id === id).result)
        result <- slick.execute(query.headOption)
      } yield result
    }

    override def getAllPlayers: Task[List[Player]] = {
      for {
        table <- ZIO.effect(new TableQuery(tag => new PlayersTable(tag)))
        query <- ZIO.succeed(table.result)
        result <- slick.execute(query).map(_.toList)
      } yield result
    }

  }

  val LIVE: URLayer[Slick, PlayerRepo] = (PlayerRepoServiceLive(_)).toLayer

}
