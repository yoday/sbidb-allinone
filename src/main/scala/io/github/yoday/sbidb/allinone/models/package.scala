package io.github.yoday.sbidb.allinone

import io.github.yoday.sbidb.allinone.services.search_engine.SearchQuery
import slick.lifted.MappedTo
import zio.json.ast.Json
import zio.json.{DeriveJsonCodec, JsonCodec}

import java.time.LocalDate

package object models {

  case class Player(
    id: PlayerId,
    firstName: String,
    lastName: String,
    position: String,
    birthDate: LocalDate,
    birthCity: String,
    birthState: String,
    college: String
  )

  case class Team(
    id: TeamId,
    mascot: String,
    city: String,
    state: String,
    owner: String,
    gm: String,
    coach: String,
    yearFounded: Int,
    stadium: String,
    division: String
  )

  case class Game(
    id: GameId,
    team1Id: TeamId,
    team2Id: TeamId,
    mvpPlayerId: PlayerId,
    mvpTeamId: TeamId,
    team1Score: Int,
    team2Score: Int,
    venueName: String,
    venueCity: String,
    venueState: String,
    gameDate: LocalDate,
    attendance: Int,
    gameNumeral: String,
    mvpBlurb: String,
    mvpStats: Json
  )

  case class PlayerId(value: Int) extends MappedTo[Int]
  case class TeamId(value: Int) extends MappedTo[Int]
  case class GameId(value: Int) extends MappedTo[Int]

  case class Players(players: List[Player])
  case class Teams(teams: List[Team])
  case class Games(games: List[Game])

  implicit val jsonJsonCodec: JsonCodec[Json] = JsonCodec(Json.encoder, Json.decoder)

  implicit val playerIdJsonCodec: JsonCodec[PlayerId] = JsonCodec.int.xmap(PlayerId.apply, _.value)
  implicit val teamIdJsonCodec: JsonCodec[TeamId] = JsonCodec.int.xmap(TeamId.apply, _.value)
  implicit val gameIdJsonCodec: JsonCodec[GameId] = JsonCodec.int.xmap(GameId.apply, _.value)

  implicit val playerJsonCodec: JsonCodec[Player] = DeriveJsonCodec.gen[Player]
  implicit val teamJsonCodec: JsonCodec[Team] = DeriveJsonCodec.gen[Team]
  implicit val gameJsonCodec: JsonCodec[Game] = DeriveJsonCodec.gen[Game]

  implicit val playersJsonCodec: JsonCodec[Players] = DeriveJsonCodec.gen[Players]
  implicit val teamsJsonCodec: JsonCodec[Teams] = DeriveJsonCodec.gen[Teams]
  implicit val gamesJsonCodec: JsonCodec[Games] = DeriveJsonCodec.gen[Games]

  case class SearchRequest(
    queryText: String
  )

  case class SearchResponse(
    queryText: String,
    searchQuery: String,
    esQuery: String,
    total: Long,
    maxScore: Double,
    results: List[SearchResult]
  )

  case class SearchResult(
    docId: String,
    entityType: String,
    entityId: String,
    score: Double,
    clauses: List[String]
  )

  implicit val searchResultJsonCodec: JsonCodec[SearchResult] = DeriveJsonCodec.gen[SearchResult]
  implicit val searchResponseJsonCodec: JsonCodec[SearchResponse] = DeriveJsonCodec.gen[SearchResponse]

  case class SearchEntityDoc(
    docId: String,
    entityType: String,
    entityId: String,
    person: List[String],
    location: List[String],
    document: List[String]
  )

  implicit val searchEntityDocJsonCodec: JsonCodec[SearchEntityDoc] = DeriveJsonCodec.gen[SearchEntityDoc]



}
