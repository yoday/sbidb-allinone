package io.github.yoday.sbidb.allinone.repositories

import io.github.yoday.sbidb.allinone.models._
import io.github.yoday.sbidb.allinone.services.slick.{Slick, SlickService}
import zio.{Function1ToLayerSyntax, Has, RIO, Task, URLayer, ZIO, ZLayer}
import io.github.yoday.sbidb.allinone.services.slick.profile.api._

package object team_repo {

  class TeamsTable(tag: Tag) extends Table[Team](tag, "teams") {
    def id = column[TeamId]("id", O.PrimaryKey)
    def mascot = column[String]("mascot")
    def city = column[String]("city")
    def state = column[String]("state")
    def owner = column[String]("owner")
    def gm = column[String]("gm")
    def coach = column[String]("coach")
    def yearFounded = column[Int]("year_founded")
    def stadium = column[String]("stadium")
    def division = column[String]("division")

    def * = (
      id,
      mascot,
      city,
      state,
      owner,
      gm,
      coach,
      yearFounded,
      stadium,
      division
    ) <> (Team.tupled, Team.unapply)
  }

  object TeamRepo {

    def findTeam(id: TeamId): RIO[TeamRepo, Option[Team]] = ZIO.accessM(_.get.findTeam(id))

    def getAllTeams: RIO[TeamRepo, List[Team]] = ZIO.accessM(_.get.getAllTeams)

  }

  type TeamRepo = Has[TeamRepoService]

  trait TeamRepoService {

    def findTeam(id: TeamId): Task[Option[Team]]

    def getAllTeams: Task[List[Team]]

  }

  case class TeamRepoServiceLive(slick: SlickService) extends TeamRepoService {

    override def findTeam(id: TeamId): Task[Option[Team]] = {
      for {
        table <- ZIO.effect(new TableQuery(tag => new TeamsTable(tag)))
        query <- ZIO.succeed(table.filter(_.id === id).result)
        result <- slick.execute(query.headOption)
      } yield result
    }

    override def getAllTeams: Task[List[Team]] = {
      for {
        table <- ZIO.effect(new TableQuery(tag => new TeamsTable(tag)))
        query <- ZIO.succeed(table.result)
        result <- slick.execute(query).map(_.toList)
      } yield result
    }

  }

  val LIVE: URLayer[Slick, TeamRepo] = (TeamRepoServiceLive(_)).toLayer

}
