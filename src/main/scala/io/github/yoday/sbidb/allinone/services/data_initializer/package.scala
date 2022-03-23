package io.github.yoday.sbidb.allinone.services

import io.github.yoday.sbidb.allinone.Utilities.{POSITION_PHRASES, STATES, convertNumeralToInt}
import io.github.yoday.sbidb.allinone.models.SearchEntityDoc
import io.github.yoday.sbidb.allinone.repositories.game_repo.{GameRepo, GameRepoService}
import io.github.yoday.sbidb.allinone.repositories.player_repo.{PlayerRepo, PlayerRepoService}
import io.github.yoday.sbidb.allinone.repositories.team_repo.{TeamRepo, TeamRepoService}
import io.github.yoday.sbidb.allinone.services.elasticsearch.{Elasticsearch, ElasticsearchService}
import io.github.yoday.sbidb.allinone.services.logging.{Logging, LoggingService}
import io.github.yoday.sbidb.allinone.services.schema_builder.SearchEntity.{docId, document}
import io.github.yoday.sbidb.allinone.services.schema_builder.{SchemaBuilder, SchemaBuilderService, SearchEntity}
import io.github.yoday.sbidb.allinone.services.slick.{Slick, SlickService}
import io.github.yoday.sbidb.allinone.services.slick.profile.api._
import zio._

package object data_initializer {

  object DataInitializer {

    def run: RIO[DataInitializer with Logging, Unit] =
      for {
        logger <- Logging.getLoggerForClass(this.getClass)
        _ <- logger.info("setting up the postgres schema")
        _ <- initPostgresSchema
        _ <- logger.info("loading the data into postgres")
        _ <- loadPostgresData
        _ <- logger.info("setting up the elasticsearch schema")
        _ <- initElasticsearchSchema
        _ <- logger.info("loading the data into elasticsearch")
        _ <- loadElasticsearchData
        _ <- logger.info("data initialization is complete")
      } yield ()

    def initPostgresSchema: RIO[DataInitializer, Unit] = ZIO.accessM(_.get.initPostgresSchema)

    def loadPostgresData: RIO[DataInitializer, Unit] = ZIO.accessM(_.get.loadPostgresData)

    def initElasticsearchSchema: RIO[DataInitializer, Unit] = ZIO.accessM(_.get.initElasticsearchSchema)

    def loadElasticsearchData: RIO[DataInitializer, Unit] = ZIO.accessM(_.get.loadElasticsearchData)

  }

  type DataInitializer = Has[DataInitializerService]

  trait DataInitializerService {

    def initPostgresSchema: Task[Unit]

    def loadPostgresData: Task[Unit]

    def initElasticsearchSchema: Task[Unit]

    def loadElasticsearchData: Task[Unit]

  }

  case class DataInitializerServiceLive(logging: LoggingService, slick: SlickService, elasticsearch: ElasticsearchService, schemaBuilder: SchemaBuilderService, playersRepo: PlayerRepoService, teamsRepo: TeamRepoService, gamesRepo: GameRepoService) extends DataInitializerService {

    override def initPostgresSchema: Task[Unit] = {
      slick.execute(SQL_TABLES_DROP).andThen(slick.execute(SQL_TABLES_CREATE)).unit
    }

    override def loadPostgresData: Task[Unit] = {
      slick.execute(SQL_DATA).unit
    }

    override def initElasticsearchSchema: Task[Unit] = {
      schemaBuilder.setupIndex(SearchEntity)
    }

    // TODO: Clean this up into multiple functions with better error handling and processing.
    override def loadElasticsearchData: Task[Unit] = {
      for {
        players <- playersRepo.getAllPlayers
        teams <- teamsRepo.getAllTeams
        games <- gamesRepo.getAllGames
        entityDocs <- ZIO.succeed {
          players.map { player =>
            SearchEntityDoc(
              docId = s"player_${player.id.value}",
              entityType = "player",
              entityId = player.id.value.toString,
              person = List(
                player.firstName,
                player.lastName,
              ),
              location = List(
                player.birthCity,
                player.birthState,
                player.college,
                STATES.getOrElse(player.birthState, ""),
              ),
              document = List(
                player.firstName,
                player.lastName,
                player.position,
                player.birthDate.toString,
                player.birthCity,
                player.birthState,
                player.college,
                STATES.getOrElse(player.birthState, ""),
                POSITION_PHRASES.get(player.position).fold("")(_.flatten.mkString(" ")),
              )
            )
          } ++ teams.map { team =>
            SearchEntityDoc(
              docId = s"team_${team.id.value}",
              entityType = "team",
              entityId = team.id.value.toString,
              person = List(
                team.mascot,
                team.owner,
                team.gm,
                team.coach,
                team.city,
                team.state,
              ),
              location = List(
                team.city,
                team.state,
                team.stadium,
                STATES.getOrElse(team.state, ""),
              ),
              document = List(
                team.mascot,
                team.owner,
                team.gm,
                team.coach,
                team.city,
                team.state,
                team.stadium,
                team.yearFounded.toString,
                team.division,
                STATES.getOrElse(team.state, ""),
              )
            )
          } ++ games.map { game =>
            SearchEntityDoc(
              docId = s"game_${game.id.value}",
              entityType = "game",
              entityId = game.id.value.toString,
              person = List(
                game.venueName,
              ),
              location = List(
                game.venueCity,
                game.venueState,
                STATES.getOrElse(game.venueState, ""),
              ),
              document = List(
                game.venueName,
                game.venueCity,
                game.venueState,
                game.team1Score.toString,
                game.team2Score.toString,
                game.gameDate.toString,
                game.attendance.toString,
                game.gameNumeral,
                convertNumeralToInt(game.gameNumeral).fold("")(_.toString),
                STATES.getOrElse(game.venueState, ""),
              ),
            )
          }
        }
        refinedEntityDocs <- ZIO.succeed { entityDocs.map { doc =>
          def refine(values: List[String]): List[String] = {
            values.mkString(" ").trim.toLowerCase.replaceAll("[^a-z0-9_ -]","").split(" ").toList.distinct
          }

          doc.copy(
            person = refine(doc.person),
            location = refine(doc.location),
            document = refine(doc.document),
          )
        }}
        logger <- logging.getLoggerForClass(this.getClass)
        _ <- ZIO.foreach_(refinedEntityDocs) { doc =>
          for {
            _ <- logger.info(s"Indexing ${doc.docId}")
            _ <- schemaBuilder.indexDocument(SearchEntity, doc)
          } yield ()
        }
        _ <- logger.info(s"Refreshing index ${SearchEntity.index.index}")
        _ <- elasticsearch.refreshIndex(SearchEntity)
        _ <- logger.info(s"index refresh done")
      } yield ()
    }

  }

  val LIVE: URLayer[Logging with Slick with Elasticsearch with SchemaBuilder with PlayerRepo with TeamRepo with GameRepo, DataInitializer] = (DataInitializerServiceLive(_,_,_,_,_,_,_)).toLayer

  final val SQL_TABLES_DROP = sqlu"""
DROP TABLE IF EXISTS games CASCADE;
DROP TABLE IF EXISTS teams CASCADE;
DROP TABLE IF EXISTS players CASCADE;
  """

  final val SQL_TABLES_CREATE = sqlu"""
CREATE TABLE players (
  id INT PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  position TEXT NOT NULL,
  birth_date DATE NOT NULL,
  birth_city TEXT NOT NULL,
  birth_state TEXT NOT NULL,
  college TEXT NOT NULL
);

CREATE TABLE teams (
  id INT PRIMARY KEY,
  mascot TEXT NOT NULL,
  city TEXT NOT NULL,
  state TEXT NOT NULL,
  owner TEXT NOT NULL,
  gm TEXT NOT NULL,
  coach TEXT NOT NULL,
  year_founded INT NOT NULL,
  stadium TEXT NOT NULL,
  division TEXT NOT NULL
);

CREATE TABLE games (
  id INT PRIMARY KEY,
  team1_id INT NOT NULL REFERENCES teams(id),
  team2_id INT NOT NULL REFERENCES teams(id),
  mvp_player_id INT NOT NULL REFERENCES players(id),
  mvp_team_id INT NOT NULL REFERENCES teams(id),
  team1_score INT NOT NULL,
  team2_score INT NOT NULL,
  venue_name TEXT NOT NULL,
  venue_city TEXT NOT NULL,
  venue_state TEXT NOT NULL,
  game_date DATE NOT NULL,
  attendance INT NOT NULL,
  game_numeral TEXT NOT NULL,
  mvp_blurb TEXT NOT NULL,
  mvp_stats JSONB NOT NULL,
  CONSTRAINT mvp_team_played_in_game CHECK (mvp_team_id = team1_id OR mvp_team_id = team2_id)
);
  """

  final val SQL_DATA = sqlu"""
-- PLAYERS
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (101,'Malcolm','Smith','OLB','1989-07-05'::DATE,'Woodland Hills','CA','Southern California');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (102,'Joe','Flacco','QB','1985-01-16'::DATE,'Audubon','NJ','Delaware');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (103,'Eli','Manning','QB','1981-01-03'::DATE,'New Orleans','LA','Mississippi');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (104,'Aaron','Rodgers','QB','1983-12-02'::DATE,'Chico','CA','California');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (105,'Drew','Brees','QB','1979-01-15'::DATE,'Austin','TX','Purdue');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (106,'Santonio','Holmes','WR','1984-03-03'::DATE,'Belle Glade','FL','Ohio State');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (107,'Peyton','Manning','QB','1976-03-24'::DATE,'New Orleans','LA','Tennessee');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (108,'Hines','Ward','WR','1976-03-08'::DATE,'Seoul','South Korea','Georgia');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (109,'Deion','Branch','WR','1979-07-18'::DATE,'Albany','GA','Louisville');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (110,'Tom','Brady','QB','1977-08-03'::DATE,'San Mateo','CA','Michigan');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (111,'Dexter','Jackson','S','1977-07-28'::DATE,'Quincy','FL','Florida State');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (112,'Ray','Lewis','MLB','1975-05-15'::DATE,'Bartow','FL','Miami');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (113,'Kurt','Warner','QB','1971-06-22'::DATE,'Burlington','IA','Northern Iowa');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (114,'John','Elway','QB','1960-06-28'::DATE,'Port Angeles','WA','Stanford');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (115,'Terrell','Davis','RB','1972-10-28'::DATE,'San Diego','CA','Georgia');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (116,'Desmond','Howard','KR','1970-05-15'::DATE,'Cleveland','OH','Michigan');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (117,'Larry','Brown','CB','1969-11-30'::DATE,'Miami','FL','Texas Christian');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (118,'Steve','Young','QB','1961-10-11'::DATE,'Salt Lake City','UT','Brigham Young');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (119,'Emmitt','Smith','RB','1969-05-15'::DATE,'Pensacola','FL','Florida');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (120,'Troy','Aikman','QB','1966-11-21'::DATE,'West Covina','CA','UCLA');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (121,'Mark','Rypien','QB','1962-10-02'::DATE,'Calgary','Canada','Washington State');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (122,'Ottis','Anderson','RB','1957-01-19'::DATE,'West Palm Beach','FL','Miami');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (123,'Joe','Montana','QB','1956-06-11'::DATE,'New Eagle','PA','Notre Dame');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (124,'Jerry','Rice','WR','1962-10-13'::DATE,'Starkville','MS','Mississippi Valley State');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (125,'Doug','Williams','QB','1955-08-09'::DATE,'Zachary','LA','Grambling State');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (126,'Phil','Simms','QB','1954-11-03'::DATE,'Springfield','KY','Morehead State');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (127,'Richard','Dent','DE','1960-12-13'::DATE,'Atlanta','GA','Tennessee State');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (128,'Marcus','Allen','RB','1960-03-26'::DATE,'San Diego','CA','Southern California');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (129,'John','Riggins','RB','1949-08-04'::DATE,'Seneca','KS','Kansas');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (130,'Jim','Plunkett','QB','1947-12-05'::DATE,'San Jose','CA','Stanford');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (131,'Terry','Bradshaw','QB','1948-09-02'::DATE,'Shreveport','LA','Lousiana Tech');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (132,'Harvey','Martin','DE','1950-11-16'::DATE,'Dallas','TX','East Texas State');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (133,'Fred','Biletnikoff','WR','1943-02-23'::DATE,'Erie','PA','Florida State');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (134,'Lynn','Swann','WR','1952-03-07'::DATE,'Alcoa','TN','Southern California');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (135,'Franco','Harris','FB','1950-03-07'::DATE,'Fort Dix','NJ','Pennsylvania State');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (136,'Larry','Csonka','FB','1946-12-25'::DATE,'Stow','OH','Syracuse');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (137,'Jake','Scott','S','1945-07-20'::DATE,'Greenwood','SC','Georgia');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (138,'Roger','Staubach','QB','1942-02-05'::DATE,'Cincinatti','OH','Navy');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (139,'Chuck','Howley','LB','1936-06-28'::DATE,'Wheeling','WV','West Virginia');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (140,'Len','Dawson','QB','1935-06-20'::DATE,'Alliance','OH','Purdue');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (141,'Joe','Namath','QB','1943-05-31'::DATE,'Beaver Falls','PA','Alabama');
INSERT INTO players (id,first_name,last_name,position,birth_date,birth_city,birth_state,college)
VALUES (142,'Bart','Starr','QB','1934-01-09'::DATE,'Montgomery','AL','Alabama');

-- TEAMS
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (201,'Seahawks','Seattle','WA','Paul Allen','John Schneider','Pete Carroll',1974,'CenturyLink Field','NFC West');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (202,'Ravens','Baltimore','MD','Steve Bisciotti','Ozzie Newsome','John Harbaugh',1996,'M&T Bank Stadium','AFC North');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (203,'Giants','New York','NJ','John Mara','Jerry Reese','Tom Coughlin',1925,'MetLife Stadium','NFC East');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (204,'Packers','Green Bay','WI','Green Bay Packers, Inc.','Ted Thompson','Mike McCarthy',1919,'Lambeau Field','NFC North');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (205,'Saints','New Orleans','LA','Tom Benson','Mickey Loomis','Sean Payton',1967,'Mercedes-Benz Superdome','NFC South');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (206,'Steelers','Pittsburgh','PA','Dan Rooney','Kevin Colbert','Mike Tomlin',1933,'Heinz Field','NFC North');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (207,'Colts','Indianapolis','IN','Jim Irsay','Ryan Grigson','Chuck Pagano',1953,'Lucas Oil Stadium','AFC South');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (208,'Patriots','New England','MA','Robert Kraft','Bill Belichick','Bill Belichick',1959,'Gillette Stadium','AFC East');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (209,'Eagles','Philadelphia','PA','Jeffrey Lurie','Howie Roseman','Chip Kelly',1933,'Lincoln Financial Field','NFC East');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (210,'Bears','Chicago','IL','Virginia Halas McCaskey','Phil Emery','Marc Trestman',1919,'Soldier Field','NFC North');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (211,'Cardinals','Arizona','AZ','Bill Bidwill','Steve Keim','Bruce Arians',1898,'University of Phoenix Stadium','NFC West');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (212,'49ers','San Francisco','CA','Jed York','Trent Baalke','Jim Harbaugh',1946,'Levi Stadium','NFC West');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (213,'Broncos','Denver','CO','Pat Bowlen','John Elway','John Fox',1960,'Sports Authority Field','AFC West');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (214,'Panthers','Carolina','NC','Jerry Richardson','Dave Gettleman','Ron Rivera',1993,'Bank of America Stadium','NFC South');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (215,'Buccaneers','Tampa Bay','FL','Malcolm Glazer','Jason Licht','Lovie Smith',1976,'Raymond James Stadium','NFC South');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (216,'Raiders','Oakland','CA','Mark Davis','Reggie McKenzie','Dennis Allen',1960,'Oakland Coliseum','AFC West');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (217,'Rams','Saint Louis','MO','Stan Kroenke','Les Snead','Jeff Fisher',1936,'Edward Jones Dome','NFC West');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (218,'Titans','Tennessee','TN','Bud Adams','Ruston Webster','Ken Whisenhunt',1960,'LP Field','AFC South');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (219,'Falcons','Atlanta','GA','Arthur Blank','Thomas Dimitroff','Mike Smith',1966,'Georgia Dome','NFC South');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (220,'Cowboys','Dallas','TX','Jerry Jones','Jerry Jones','Jason Garrett',1960,'AT&T Stadium','NFC East');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (221,'Chargers','San Diego','CA','Alex Spanos','Tom Telesco','Mike McCoy',1960,'Qualcomm Stadium','AFC West');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (222,'Bills','Buffalo','NY','Mary Wilson','Doug Whaley','Doug Marrone',1960,'Ralph Wilson Stadium','AFC East');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (223,'Redskins','Washington','MD','Dan Snyder','Bruce Allen','Jay Gruden',1932,'FedEx Field','NFC East');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (224,'Bengals','Cincinatti','OH','Mike Brown','Mike Brown','Marvin Lewis',1968,'Paul Brown Stadium','NFC North');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (225,'Dolphins','Miami','FL','Stephen Ross','Dennis Hickey','Joe Philbin',1966,'Sun Life Stadium','AFC East');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (226,'Vikings','Minnesota','MN','Zygi Wilf','Rick Spielman','Mike Zimmer',1961,'TCF Bank Stadium','NFC North');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (227,'Jets','New York','NJ','Woody Johnson','John Idzik','Rex Ryan',1963,'MetLife Stadium','AFC East');
INSERT INTO teams (id,mascot,city,state,owner,gm,coach,year_founded,stadium,division)
VALUES (228,'Chiefs','Kansas City','MO','Clark Hunt','John Dorsey','Andy Reid',1963,'Arrowhead Stadium','AFC West');

-- GAMES
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (301,208,209,109,208,24,21,'Alltel Stadium','Jacksonville','FL','2005-02-06'::DATE,78125,'XXXIX','Deion Branch became the third offensive player to win the SB MVP without accounting for a touchdown. His 11 receptions tied a Super Bowl record.','{"recvTargeted":12,"recvTouchdowns":0,"recvYards":122,"recvReceptions":11}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (302,206,201,108,206,21,10,'Ford Field','Detroit','MI','2006-02-05'::DATE,68206,'XL','Legendary Steeler Hines Ward lifted the Steelers to their 5th Super Bowl victory while winning his only Super Bowl MVP. Known as a ferocious blocker, Ward led the Steelers in receptions and yards, acting as a security blanket for second-year QB Ben Roethlisberger.','{"rushYards":18,"rushCarries":1,"recvTargeted":11,"recvTouchdowns":1,"recvYards":123,"recvReceptions":5}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (303,207,210,107,207,29,17,'Dolphin Stadium','Miami Gardens','FL','2007-02-04'::DATE,74512,'XLI','Future Hall of Famer Peyton Manning led the Indianapolis Colts to their second Super Bowl championship. Manning capped off a season in which he threw 31 TD and only 9 INT by throwing for 1 TD and 1 INT','{"rushYards":0,"rushCarries":1,"passInterceptions":1,"passTouchdowns":1,"passYards":247,"passAttempts":38,"passCompletions":25}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (304,203,208,103,203,17,14,'University of Phoenix Stadium','Glendale','AZ','2008-02-03'::DATE,71101,'XLII','Eli Manning and the Giants upset the heavily favored Patriots on a late game heave to WR David Tyree. His 2 fourth quarter TDs lifted the Giants over the Patriots, who had yet to lose a game that season.','{"rushYards":4,"rushCarries":3,"passInterceptions":1,"passTouchdowns":2,"passYards":255,"passAttempts":34,"passCompletions":19}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (305,206,211,106,206,27,23,'Raymond James Stadium','Tampa','FL','2009-02-01'::DATE,70774,'XLIII','Santonio Holmes game winning 6 yd TD catch resulted in the Steelers'' sixth Super Bowl championship. The catch was one of the all time greats in SB history, and was one of his nine spectacular receptions that game.','{"recvTargeted":13,"recvTouchdowns":1,"recvYards":131,"recvReceptions":9}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (306,205,207,105,205,31,17,'Sun Life Stadium','Miami Gardens','FL','2010-02-07'::DATE,74059,'XLIV','Four years after Hurricane Katrina devastated New Orleans, Future Hall of Famer Drew Brees led the Saints to their first Super Bowl championship. His 32 completions tied Tom Brady''s record for the most in Super Bowl history.','{"rushYards":-1,"rushCarries":1,"passInterceptions":0,"passTouchdowns":2,"passYards":288,"passAttempts":39,"passCompletions":32}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (307,204,206,104,204,31,25,'Cowboys Stadium','Arlington','TX','2011-02-06'::DATE,103219,'XLV','In his third year as a starter for the Green Bay Packers, Future Hall of Famer Rodgers calmly led the team its fourth Super Bowl championship. His 3 TDs reminded fans of the retired Brett Favre and ensured Packers fans of success in years to come.','{"rushYards":-2,"rushCarries":2,"passInterceptions":0,"passTouchdowns":3,"passYards":304,"passAttempts":39,"passCompletions":24}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (308,203,208,103,203,21,17,'Lucas Oil Stadium','Indianapolis','IN','2012-02-05'::DATE,68658,'XLVI','Once again, Eli Manning played spoiler to the New England Patriots and won his second SB MVP award. He started the game completing his first 9 attempts becoming the first quarterback to do so and put together a crucial drive with 3 1/2 minutes left that sealed the victory for the Giants.','{"rushYards":-1,"rushCarries":1,"passInterceptions":0,"passTouchdowns":1,"passYards":296,"passAttempts":40,"passCompletions":30}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (309,202,212,102,202,34,31,'Mercedes-Benz Superdome','New Orleans','LA','2013-02-03'::DATE,71024,'XLVII','Joe Flacco capped his spectacular postseason with an impressive performance against a 49er defense that was considered one of the league''s best. While all 3 TDs were in the first half, Flacco coolly converted third down after third down to hold off a late 49er surge.','{"rushYards":0,"rushCarries":0,"passInterceptions":0,"passTouchdowns":3,"passYards":287,"passAttempts":33,"passCompletions":22}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (310,201,213,101,201,43,8,'MetLife Stadium','East Rutherford','NJ','2014-02-02'::DATE,82529,'XLVIII','Malcolm Smith was the de-facto SB MVP for a legendary Seattle defense. While he was a relative unknown at the start of the season, a game-sealing interception in the NFC Championship as well as two takeaways in the Super Bowl served as a coming out party for the young linebacker.','{"defSoloTackles":6,"defCombinedTackles":10,"defTouchdowns":1,"defInterceptions":1,"defTacklesForLoss":9}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (311,208,214,110,208,32,29,'Reliant Stadium','Houston','TX','2004-02-01'::DATE,71525,'XXXVIII','Future Hall of Famer Tom Brady led the Patriots to victory while winning his second Super Bowl MVP. His 32 completions are the most in SB history and his 354 yds are the 5th best total in SB history.','{"rushYards":12,"rushCarries":2,"passInterceptions":1,"passTouchdowns":3,"passYards":354,"passAttempts":48,"passCompletions":32}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (312,215,216,111,215,48,21,'Qualcomm Stadium','San Diego','CA','2003-01-26'::DATE,67603,'XXXVII','Dexter Jackson led the Tampa Bay Buccaneers to their first and only Super Bowl victory with two interceptions. His two interceptions were part of five for a defense that largely shut down the Oakland Raiders.','{"defInterceptions":2,"defTacklesForLoss":2}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (313,208,217,110,208,20,17,'Louisiana Superdome','New Orleans','LA','2002-02-03'::DATE,72922,'XXXVI','Future Hall of Famer and second year QB Tom Brady led an underdog Patriots to a Super Bowl championship where he managed to beat the St. Louis Rams and the "Greatest Show on Turf" offense. His passing yardage was third-lowest in SB history, but this year was the first year that included fan votes for SB MVP.','{"rushYards":3,"rushCarries":1,"passInterceptions":0,"passTouchdowns":1,"passYards":145,"passAttempts":27,"passCompletions":16}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (314,202,203,112,202,34,7,'Raymond James Stadium','Tampa Bay','FL','2001-01-28'::DATE,71921,'XXXV','Future Hall of Famer LB Ray Lewis led the Ravens defense that limited the Giants to only 152 yards. While his stat line was unimpressive, his extraordinary leadership for a legendary defense garnered him the award.','{"defPassesDefended":4,"defTacklesForLoss":3}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (315,217,218,113,217,23,16,'Georgia Dome','Atlanta','GA','2000-01-31'::DATE,72625,'XXXIV','Kurt Warner led the St. Louis Rams and the Greatest Show on Turf offense to first and only Super Bowl championship. His 414 yards are the most in Super Bowl history, and he became the sixth player to win the Super Bowl and Regular Season MVP awards in the same season.','{"rushYards":1,"rushCarries":1,"passInterceptions":0,"passTouchdowns":2,"passYards":414,"passAttempts":45,"passCompletions":24}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (316,213,219,114,213,31,19,'Pro Player Stadium','Miami','FL','1999-01-31'::DATE,74803,'XXXIII','Hall of Famer John Elway led the Denver Broncos to their second consecutive Super Bowl championship with his gritty performance. He was the oldest Super Bowl MVP at 38 years old and retired after the championship.','{"rushTouchdowns":1,"rushYards":2,"rushCarries":3,"passInterceptions":1,"passTouchdowns":1,"passYards":336,"passAttempts":29,"passCompletions":18}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (317,213,204,115,213,31,24,'Qualcomm Stadium','San Diego','CA','1998-01-25'::DATE,68912,'XXXII','Despite missing the most of the second quarter, Terrell Davis came back and scored the game winning touchdown with 1:45 left to play. He returned home to win his first Super Bowl, and his 3 rushing touchdowns are a Super Bowl record.','{"recvYards":8,"recvReceptions":2,"rushTouchdowns":3,"rushYards":157,"rushCarries":30}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (318,204,208,116,204,35,21,'Louisiana Superdome','New Orleans','LA','1997-01-26'::DATE,72301,'XXXI','Electrifying athlete Desmond Howard became the first special teams player to win the Super Bowl MVP after sealing the game with a 99 yd kick off return for a touchdown. He went on to become one of the most feared returners in the league, picking up right where he left off as a Heisman trophy winner in college.','{"puntYards":90,"puntReturns":6,"kickTouchdowns":1,"kickYards":154,"kickReturns":4}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (319,220,206,117,220,27,17,'Sun Devil Stadium','Tempe','AZ','1996-01-28'::DATE,76347,'XXX','Dallas'' Larry Brown became the first cornerback to win the Super Bowl MVP by recording two interceptions that led to Emmitt Smith touchdowns. His two interceptions helped the Cowboys rally back from two deficits and secured Dallas'' fifth Super Bowl Championship.','{"defYards":77,"defInterceptions":2}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (320,212,221,118,212,49,26,'Joe Robbie Stadium','Miami','FL','1995-01-29'::DATE,74107,'XXIX','Hall of Famer Steve Young led the Niners to their fifth Super Bowl championship by throwing for a record 6 TDs. He led the Niners in both passing and rushing yards becoming the first Super Bowl MVP to lead his team in both categories.','{"rushYards":49,"rushCarries":5,"passInterceptions":0,"passTouchdowns":6,"passYards":325,"passAttempts":36,"passCompletions":24}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (321,220,222,119,220,30,13,'Georgia Dome','Atlanta','GA','1994-01-30'::DATE,72817,'XXVIII','Hall of Famer Emmitt Smith turned in a monster performance to lead the Dallas Cowboys to their fourth Super Bowl Championship. His 158 yards from scrimmage accounted for nearly half of the Cowboys'' yardage.','{"recvYards":26,"recvReceptions":4,"rushTouchdowns":2,"rushYards":132,"rushCarries":30}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (322,220,222,120,220,52,17,'Rose Bowl Stadium','Pasadena','CA','1993-01-31'::DATE,98374,'XXVII','Hall of Famer Troy Aikman helped the Cowboys win their third Super Bowl Championship with his only Super Bowl MVP in an illustrious career. It was performances like these that grant him the title of the Super Bowl''s most accurate passer with a completion percentage of 70.0.','{"rushYards":28,"rushCarries":3,"passInterceptions":0,"passTouchdowns":4,"passYards":273,"passAttempts":30,"passCompletions":22}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (323,223,222,121,223,37,24,'Hubert Humphrey Metrodome','Minneapolis','MN','1992-01-26'::DATE,63130,'XXVI','As one of the lowest paid quarterbacks in the modern era, Rypien and his great stable of receivers brought the Redskins their third championship. Rypien became the third different quarterback to win a Super Bowl with the Redskins.','{"rushYards":-4,"rushCarries":6,"passInterceptions":1,"passTouchdowns":2,"passYards":292,"passAttempts":33,"passCompletions":18}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (324,203,222,122,203,20,19,'Tampa Stadium','Tampa Bay','FL','1991-01-27'::DATE,73813,'XXV','In the last Super Bowl at Tampa Stadium (before it was demolished), Ottis Anderson led a Giants rushing attack whose ability to hold position kept the explosive Bills offense off the field. Anderson''s ability to pick up first downs allowed the Giants to hold the ball for nearly 40 minutes.','{"recvYards":7,"recvReceptions":1,"rushTouchdowns":1,"rushYards":102,"rushCarries":21}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (325,212,213,123,212,55,10,'Louisiana Superdome','New Orleans','LA','1990-01-28'::DATE,72919,'XXIV','Hall of Famer Joe Montana won his third Super Bowl MVP and his fourth Super Bowl with his performance against the Broncos. At one point, he completed a then Super Bowl record 13 straight passes, and he became the third player to win both the regular season MVP and Super Bowl MVP in the same season. His 5 TDs are the second most in Super Bowl history.','{"rushYards":15,"rushCarries":2,"passInterceptions":0,"passTouchdowns":5,"passYards":297,"passAttempts":29,"passCompletions":22}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (326,212,224,124,212,20,16,'Joe Robbie Stadium','Miami','FL','1989-01-22'::DATE,75597,'XXIII','Hall of Famer Jerry Rice set a Super Bowl record with 215 receiving yards in the 49ers'' third Super Bowl victory. Performances like these made him one of the most accomplished wide receivers in the history of the Super Bowl.','{"rushYards":5,"rushCarries":1,"recvTouchdowns":1,"recvYards":215,"recvReceptions":11}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (327,223,213,125,223,42,10,'Jack Murphy Stadium','San Diego','CA','1988-01-31'::DATE,73302,'XXII','After starting the season as a backup, Doug Williams led the Redskins to a beatdown of John Elway''s Broncos while becoming the first African American quarterback in Super Bowl history. He is the only quarterback to have thrown for 4 TDs in a quarter, and his 340 YDs were a Super Bowl record.','{"rushYards":-2,"rushCarries":2,"passInterceptions":1,"passTouchdowns":4,"passYards":340,"passAttempts":29,"passCompletions":18}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (328,203,213,126,203,39,20,'Rose Bowl Stadium','Pasadena','CA','1987-01-25'::DATE,101063,'XXI','Phil Simms and the "Big Blue Wrecking Crew" of the New York Giants demolished John Elway''s Broncos in Super Bowl XXI. Simms'' accuracy (he posted an 88.0 completion percentage, a Super Bowl record) and a third quarter offensive explosion led the Giants to their first of four Super Bowl championships.','{"rushYards":25,"rushCarries":3,"passInterceptions":0,"passTouchdowns":3,"passYards":268,"passAttempts":25,"passCompletions":22}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (329,210,208,127,210,46,10,'Louisiana Superdome','New Orleans','LA','1986-01-26'::DATE,73818,'XX','Hall of Famer Richard Dent led a Bears defense that held the Patriots to 123 total yards. The Dent-led defense set Super Bowl records for sacks (7), fewest rush yards allowed (7), and margin of victory (36 points).','{"defPassesDefended":1,"defForcedFumbles":2,"defSacks":2}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (330,212,225,123,212,38,16,'Stanford Stadium','Stanford','CA','1985-01-20'::DATE,84059,'XIX','In a dominating fashion, Joe Montana coordinated the 49ers'' offense to a demolition of the Miami Dolphins. His 59 rushing yards were a Super Bowl record for a QB. It was his second Super Bowl MVP award.','{"rushTouchdowns":1,"rushYards":59,"rushCarries":5,"passInterceptions":0,"passTouchdowns":3,"passYards":331,"passAttempts":35,"passCompletions":24}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (331,216,223,128,216,38,9,'Tampa Stadium','Tampa Bay','FL','1984-01-22'::DATE,72920,'XVIII','Marcus Allen put the Los Angeles Raiders (at the time) on his back and brought home the trophy in his only Super Bowl appearance in an illustrious Hall of Fame career. His performance in the game yielded him the most rushing yards in Super Bowl history at the time, and his 9.6 yards per carry is an all-time Super Bowl record.','{"recvYards":18,"recvReceptions":2,"rushTouchdowns":2,"rushYards":191,"rushCarries":20}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (332,223,225,129,223,27,17,'Rose Bowl Stadium','Pasadena','CA','1983-01-30'::DATE,103667,'XVII','With the Dolphins coming into the game as a slight favorite, the Redskins maintained possession, ran for a record 276 yards, and won the game thanks to the efforts of Hall of Famer John Riggins. His go-ahead 43 YD touchdown gave him a the Super Bowl record for rushing yards, and his 38 carries are an all-time Super Bowl record.','{"recvYards":15,"recvReceptions":1,"rushTouchdowns":1,"rushYards":166,"rushCarries":38}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (333,212,224,123,212,26,21,'Pontiac Silverdome','Pontiac','MI','1982-01-24'::DATE,81270,'XVI','In his most unspectacular Super Bowl performance, Joe Montana led the 49ers to their first Super Bowl title. While they were outgained in both yards and touchdowns, the Niners prevailed mainly due to Montana''s resilience that made him a 4 time Super Bowl champion.','{"rushTouchdowns":1,"rushYards":18,"rushCarries":6,"passInterceptions":0,"passTouchdowns":1,"passYards":157,"passAttempts":22,"passCompletions":14}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (334,216,209,130,216,27,10,'Louisiana Superdome','New Orleans','LA','1981-01-25'::DATE,76135,'XV','After bouncing around the league since he arrived as a Heisman Trophy winner, Plunkett was signed to be a back up for the Raiders. When their starting QB went down with an injury, Plunkett stepped up and revived his career by winning his first Super Bowl title at age 33. He became the second Heisman winner to win a Super Bowl MVP.','{"rushYards":9,"rushCarries":3,"passInterceptions":0,"passTouchdowns":3,"passYards":261,"passAttempts":21,"passCompletions":13}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (335,206,217,131,206,31,19,'Rose Bowl','Pasadena','CA','1980-01-20'::DATE,103985,'XIV','Terry Bradshaw became the first to earn back-to-back MVP honors since Green Bay''s Bart Starr. In a 31-19 victory over the Rams, Bradshaw threw for 309 yards and two touchdowns, completing 14-of-21 passes. Bradshaw twice rallied Pittsburgh from behind, including a 13-10 deficit at the half. Bradshaw teamed with John Stallworth on a 73-yard score to take back the lead for good.','{"rushYards":9,"rushCarries":3,"passInterceptions":3,"passTouchdowns":2,"passYards":309,"passAttempts":21,"passCompletions":14}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (336,206,220,131,206,35,31,'Orange Bowl','Miami','FL','1979-01-21'::DATE,79484,'XIII','Throwing a Super Bowl record four touchdown passes, quarterback Terry Bradshaw was named the MVP in the Steelers'' 35-31 win over Dallas. Setting a personal best with 318 passing yards, Bradshaw led Pittsburgh to its third Super Bowl championship in five seasons. With 17 completions in 30 attempts, nearly 25 percent of Bradshaw''s passes went for scores.','{"rushTouchdowns":0,"rushYards":-5,"rushCarries":1,"passInterceptions":1,"passTouchdowns":4,"passYards":318,"passAttempts":30,"passCompletions":17}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (337,220,213,132,220,27,10,'Louisiana Superdome','New Orleans','LA','1978-01-15'::DATE,76400,'XII','Defensive tackle Randy White was one of the leaders of a Dallas defense that forced eight Denver turnovers. Bronco quarterbacks had only 8 completions in 25 attempts under the fierce Cowboy rush, led by White.','{"defSacks":1}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (338,216,226,133,216,32,14,'Rose Bowl','Pasadena','CA','1977-01-09'::DATE,103438,'XI','Eye-black blazing and wristbands flashing, wide receiver Fred Biletnikoff helped Oakland win its first NFL title with a 32-14 victory over Minnesota. While Biletnikoff had only 4 receptions for 79 yards, three of his catches set up short Oakland touchdowns.','{"recvTouchdowns":0,"recvYards":79,"recvReceptions":4}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (339,206,220,134,206,21,17,'Orange Bowl','Miami','FL','1976-01-18'::DATE,80187,'X','The Steelers were led by balletic wide receiver Lynn Swann, who set a Super Bowl record with 161 receiving yards on 4 catches and earned the MVP award. A 64-yard touchdown pass from future two-time Super Bowl MVP Terry Bradshaw to Swann late in the fourth quarter proved to be the decisive score.','{"recvTouchdowns":1,"recvYards":161,"recvReceptions":4}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (340,206,226,135,206,16,6,'Tulane Stadium','New Orleans','LA','1975-01-12'::DATE,80997,'IX','To earn the first of its four Super Bowl championships, Pittsburgh turned to workhorse running back Franco Harris. Harris rushed 34 times for 158 yards, breaking the record Larry Csonka set one year earlier. After a baseball-like 2-0 halftime score in favor of Pittsburgh, the Steelers took advantage with a Minnesota fumble. Harris'' running and the powerful Steeler defense combined to make that lead stand.','{"rushTouchdowns":1,"rushYards":158,"rushCarries":34}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (341,225,226,136,225,24,7,'Rice Stadium','Houston','TX','1974-01-13'::DATE,71882,'VIII','Powerful running back Larry Csonka powered Miami to its second consecutive Super Bowl championship, a 24-7 victory over Minnesota. Csonka carried the ball 33 times for 145 yards, a Super Bowl record at the time. Csonka''s 5-yard touchdown run in the first quarter capped off a 62-yard drive and opened the scoring for the Dolphins. Ahead 17-0 in the third quarter, Miami put the game away on with a Csonka 2-yard run.','{"rushTouchdowns":2,"rushYards":145,"rushCarries":33}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (342,225,223,137,225,14,7,'Los Angeles Memorial Coliseum','Los Angeles','CA','1973-01-14'::DATE,90182,'VII','Safety Jake Scott became only the second defensive player to win the MVP. Scott had two interceptions, including one in the end zone during the fourth quarter. That interception and his 55-yard return iced the game for the Dolphins.','{"defSoloTackles":2,"defCombinedTackles":2,"defYards":63,"defInterceptions":2}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (343,220,225,138,220,24,3,'Tulane Stadium','New Orleans','LA','1972-01-16'::DATE,81023,'VI','Quarterback Roger Staubach earned the MVP award for directing an attack where he completed 12 of 19 passes for 119 yards and 2 touchdowns. Leading 3-0, Dallas made it 10-0 on Staubach''s 7-yard touchdown pass to Lance Alworth. Staubach finished off the scoring in the fourth quarter with another 7-yard touchdown pass.','{"passTouchdowns":2,"passYards":119,"passAttempts":19,"passCompletions":12}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (344,207,220,139,220,16,13,'Orange Bowl','Miami','FL','1971-01-17'::DATE,80562,'V','Dallas linebacker Chuck Howley became the first defensive player to be named Super Bowl MVP. But the honor had a hollow ring for Howley, who also became the first player from a losing team to be named MVP. Howley intercepted two passes and receovered a fumble to win the honor, although his effort was overshadowed by Baltimore''s eventual win.','{"defSoloTackles":2,"defCombinedTackles":2,"defYards":22,"defInterceptions":2}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (345,228,226,140,228,23,7,'Tulane Stadium','New Orleans','LA','1970-01-11'::DATE,80562,'IV','A series of Dawson-led drives took Kansas City to three field goals before scoring on a short run to give the Chiefs a 16-0 halftime lead. In the fourth quarter, Dawson delivered the clinching score at the end of an 82-yard drive, hitting wide receiver Otis Taylor with a 46-yard touchdown pass.','{"rushYards":11,"rushCarries":3,"passInterceptions":1,"passTouchdowns":1,"passYards":142,"passAttempts":17,"passCompletions":12}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (346,227,207,141,227,16,7,'Orange Bowl','Miami','FL','1969-01-12'::DATE,75389,'III','Completing 17-of-28 passes for 206 yards, Namath directed a Jets attack that rolled up 337 yards of total offense. Namath''s famous run off the field after the game, his index finger waving to let the world know who was No. 1, is one of the enduring images of the Super Bowl.','{"passInterceptions":0,"passTouchdowns":0,"passYards":206,"passAttempts":28,"passCompletions":17}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (347,204,216,142,204,35,10,'Los Angeles Memorial Coliseum','Los Angeles','CA','1967-01-15'::DATE,61946,'I','In leading the Packers to a 35-10 victory over Kansas City, Starr completed 16 of 23 passes for 250 yards and three touchdowns. His main target in Super Bowl I was reserve wide receiver Max McGee, who caught 7 passes for 138 yards. McGee and Starr hooked up in the first quarter for a 37-yard score, and again at the end of the third quarter for a 13-yard touchdown.','{"passInterceptions":1,"passTouchdowns":2,"passYards":250,"passAttempts":23,"passCompletions":16}'::JSONB);
INSERT INTO games (id,team1_id,team2_id,mvp_player_id,mvp_team_id,team1_score,team2_score,venue_name,venue_city,venue_state,game_date,attendance,game_numeral,mvp_blurb,mvp_stats)
VALUES (348,204,228,142,204,33,14,'Orange Bowl','Miami','FL','1968-01-14'::DATE,75546,'II','After a pair of field goals gave Green Bay an early lead, Starr threw a 62-yard touchdown pass to Boyd Dowler. Leading 16-7 at the half, the Packers scored 17 unanswered second-half points to take a commanding lead. Starr completed 13-of-24 passes for 202 yards and 1 touchdown.','{"rushYards":14,"rushCarries":1,"passInterceptions":0,"passTouchdowns":1,"passYards":202,"passAttempts":24,"passCompletions":13}'::JSONB);
  """

}
