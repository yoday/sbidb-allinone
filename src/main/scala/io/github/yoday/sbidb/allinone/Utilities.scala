package io.github.yoday.sbidb.allinone

import info.debatty.java.stringsimilarity.Levenshtein

object Utilities {

  // Sure, a proper algorithm to convert is the right thing to do, but this is not important enough to waste brain cycles on.
  def convertNumeralToInt(value: String): Option[Int] = value match {
    case "I" => Some(1)
    case "II" => Some(2)
    case "III" => Some(3)
    case "IV" => Some(4)
    case "V" => Some(5)
    case "VI" => Some(6)
    case "VII" => Some(7)
    case "VIII" => Some(8)
    case "IX" => Some(9)
    case "X" => Some(10)
    case "XI" => Some(11)
    case "XII" => Some(12)
    case "XIII" => Some(13)
    case "XIV" => Some(14)
    case "XV" => Some(15)
    case "XVI" => Some(16)
    case "XVII" => Some(17)
    case "XVIII" => Some(18)
    case "XIX" => Some(19)
    case "XX" => Some(20)
    case "XXI" => Some(21)
    case "XXII" => Some(22)
    case "XXIII" => Some(23)
    case "XXIV" => Some(24)
    case "XXV" => Some(25)
    case "XXVI" => Some(26)
    case "XXVII" => Some(27)
    case "XXVIII" => Some(28)
    case "XXIX" => Some(29)
    case "XXX" => Some(30)
    case "XXXI" => Some(31)
    case "XXXII" => Some(32)
    case "XXXIII" => Some(33)
    case "XXXIV" => Some(34)
    case "XXXV" => Some(35)
    case "XXXVI" => Some(36)
    case "XXXVII" => Some(37)
    case "XXXVIII" => Some(38)
    case "XXXIX" => Some(39)
    case "XL" => Some(40)
    case "XLI" => Some(41)
    case "XLII" => Some(42)
    case "XLIII" => Some(43)
    case "XLIV" => Some(44)
    case "XLV" => Some(45)
    case "XLVI" => Some(46)
    case "XLVII" => Some(47)
    case "XLVIII" => Some(48)
    case "XLIX" => Some(49)
    case "L" => Some(50)
    case "LI" => Some(51)
    case "LII" => Some(52)
    case "LIII" => Some(53)
    case "LIV" => Some(54)
    case "LV" => Some(55)
    case "LVI" => Some(56)
    case "LVII" => Some(57)
    case "LVIII" => Some(58)
    case "LIX" => Some(59)
    case "LX" => Some(60)
    case "LXI" => Some(61)
    case "LXII" => Some(62)
    case "LXIII" => Some(63)
    case "LXIV" => Some(64)
    case "LXV" => Some(65)
    case "LXVI" => Some(66)
    case "LXVII" => Some(67)
    case "LXVIII" => Some(68)
    case "LXIX" => Some(69)
    case "LXX" => Some(70)
    case "LXXI" => Some(71)
    case "LXXII" => Some(72)
    case "LXXIII" => Some(73)
    case "LXXIV" => Some(74)
    case "LXXV" => Some(75)
    case "LXXVI" => Some(76)
    case "LXXVII" => Some(77)
    case "LXXVIII" => Some(78)
    case "LXXIX" => Some(79)
    case "LXXX" => Some(80)
    case "LXXXI" => Some(81)
    case "LXXXII" => Some(82)
    case "LXXXIII" => Some(83)
    case "LXXXIV" => Some(84)
    case "LXXXV" => Some(85)
    case "LXXXVI" => Some(86)
    case "LXXXVII" => Some(87)
    case "LXXXVIII" => Some(88)
    case "LXXXIX" => Some(89)
    case _ => None
  }

  // Sure, a proper algorithm to convert is the right thing to do, but this is not important enough to waste brain cycles on.
  def convertIntToNumeral(value: Int): Option[String] = value match {
    case 1 => Some("I")
    case 2 => Some("II")
    case 3 => Some("III")
    case 4 => Some("IV")
    case 5 => Some("V")
    case 6 => Some("VI")
    case 7 => Some("VII")
    case 8 => Some("VIII")
    case 9 => Some("IX")
    case 10 => Some("X")
    case 11 => Some("XI")
    case 12 => Some("XII")
    case 13 => Some("XIII")
    case 14 => Some("XIV")
    case 15 => Some("XV")
    case 16 => Some("XVI")
    case 17 => Some("XVII")
    case 18 => Some("XVIII")
    case 19 => Some("XIX")
    case 20 => Some("XX")
    case 21 => Some("XXI")
    case 22 => Some("XXII")
    case 23 => Some("XXIII")
    case 24 => Some("XXIV")
    case 25 => Some("XXV")
    case 26 => Some("XXVI")
    case 27 => Some("XXVII")
    case 28 => Some("XXVIII")
    case 29 => Some("XXIX")
    case 30 => Some("XXX")
    case 31 => Some("XXXI")
    case 32 => Some("XXXII")
    case 33 => Some("XXXIII")
    case 34 => Some("XXXIV")
    case 35 => Some("XXXV")
    case 36 => Some("XXXVI")
    case 37 => Some("XXXVII")
    case 38 => Some("XXXVIII")
    case 39 => Some("XXXIX")
    case 40 => Some("XL")
    case 41 => Some("XLI")
    case 42 => Some("XLII")
    case 43 => Some("XLIII")
    case 44 => Some("XLIV")
    case 45 => Some("XLV")
    case 46 => Some("XLVI")
    case 47 => Some("XLVII")
    case 48 => Some("XLVIII")
    case 49 => Some("XLIX")
    case 50 => Some("L")
    case 51 => Some("LI")
    case 52 => Some("LII")
    case 53 => Some("LIII")
    case 54 => Some("LIV")
    case 55 => Some("LV")
    case 56 => Some("LVI")
    case 57 => Some("LVII")
    case 58 => Some("LVIII")
    case 59 => Some("LIX")
    case 60 => Some("LX")
    case 61 => Some("LXI")
    case 62 => Some("LXII")
    case 63 => Some("LXIII")
    case 64 => Some("LXIV")
    case 65 => Some("LXV")
    case 66 => Some("LXVI")
    case 67 => Some("LXVII")
    case 68 => Some("LXVIII")
    case 69 => Some("LXIX")
    case 70 => Some("LXX")
    case 71 => Some("LXXI")
    case 72 => Some("LXXII")
    case 73 => Some("LXXIII")
    case 74 => Some("LXXIV")
    case 75 => Some("LXXV")
    case 76 => Some("LXXVI")
    case 77 => Some("LXXVII")
    case 78 => Some("LXXVIII")
    case 79 => Some("LXXIX")
    case 80 => Some("LXXX")
    case 81 => Some("LXXXI")
    case 82 => Some("LXXXII")
    case 83 => Some("LXXXIII")
    case 84 => Some("LXXXIV")
    case 85 => Some("LXXXV")
    case 86 => Some("LXXXVI")
    case 87 => Some("LXXXVII")
    case 88 => Some("LXXXVIII")
    case 89 => Some("LXXXIX")
    case _ => None
  }

  def makeBootstrapPage(innerHead: String, innerBody: String) = s"""
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <!-- Bootstrap CSS -->
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" crossorigin="anonymous">

    $innerHead

  </head>
  <body>
    <div class="container">
      <div class="row">
        <div class="col" align="center">
          <h1>SuperBowl Internet DataBase</h1>
        </div>
      </div>
      <div class="row">
        <div class="col">

        <form method="GET" action="/search">
          <div class="container">
            <div class="row form-group">
              <div class="col-1">
                <a href="/home">HOME</a>
              </div>
              <div class="col-10">
                <input type="search" name="q" placeholder="Search our database!" class="form-control" style="width: 100%" value="{{queryText}}" />
              </div>
              <div class="col-1">
                <button class="btn btn-outline-success" type="submit">Search</button>
              </div>
            </div>
          </div>
        </form>

        </div>
      </div>
      <hr />
      <div class="row">
        <div class="col">

          $innerBody

        </div>
      </div>
    </div>

    <!-- Bootstrap Bundle with Popper -->
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p" crossorigin="anonymous"></script>
  </body>
</html>
"""

  // Compare two strings for equality using a string similarity algorithm.
  def fuzzyEquals(stringA: String, stringB: String, editDistance: Int = 2): Boolean = {
    val distance = new Levenshtein().distance(stringA, stringB, editDistance)
    0D <= distance && distance <= editDistance
  }

  // Compare if the fullList starts with all the items of the prefixList using the fuzzy equality function
  def isFuzzyListPrefix(fullList: List[String], prefixList: List[String], editDistance: Int = 2): Boolean = {
    (fullList, prefixList) match {
      case (Nil, Nil) =>
        true
      case (_ :: _, Nil) =>
        true
      case (Nil, _ :: _) =>
        false
      case (fullHead :: fullTail, prefixHead :: prefixTail) =>
        fuzzyEquals(fullHead, prefixHead, editDistance) && isFuzzyListPrefix(fullTail, prefixTail, editDistance)
    }
  }

  final val SYNONYMS = List(
    Set("offense", "offenses", "offensive"),
    Set("defense", "defenses", "defensive", "defended", "def"),
    Set("combo", "combos", "combined", "combination", "combinations", "together", "grouped"),
    Set("tackle", "tackles", "tackler", "tacklers", "tackling", "tackled", "tkl", "tkls"),
    Set("force", "forces", "forced", "forcing"),
    Set("interception", "interceptions", "intercept", "intercepts", "intercepted", "int", "ints"),
    Set("pass", "passes", "passing", "passed", "thrown", "throwing"),
    Set("sack", "sacks", "sacked", "sacking"),
    Set("solo", "solos", "soloed", "individual", "individuals", "single", "singular"),
    Set("loss", "lost", "lose", "losing"),
    Set("touchdown", "touchdowns", "down", "downs", "td", "tds"),
    Set("yard", "yards", "yd", "yds"),
    Set("kick", "kicks", "kicked", "kicking"),
    Set("return", "returns", "returning", "returned", "ret", "rets"),
    Set("punt", "punts", "punted", "punting"),
    Set("attempt", "attempts", "attempted", "attempting", "att", "atts", "try", "tries", "trying", "tried"),
    Set("complete", "completed", "completions", "completing"),
    Set("reception", "receptions", "receive", "received", "caught", "recv"),
    Set("target", "targets", "targeted", "targeting", "tgt", "tgts"),
    Set("carry", "carries", "carrying", "carried"),
    Set("rush", "rushes", "rushed", "rushing"),
    Set("american", "america"),
    Set("national", "nation"),
    Set("run", "running", "ran", "runner", "runners")
  )

  final val STATS_PHRASES = Map(
    "defCombinedTackles" -> List(
      List("defense", "combined", "tackles"),
      List("combined", "tackles"),
      List("tackles", "combined"),
      List("tackles"),
    ),
    "defForcedFumbles" -> List(
      List("defense", "forced", "fumbles"),
      List("forced", "fumbles"),
      List("fumbles", "forced"),
      List("fumbles"),
    ),
    "defInterceptions" -> List(
      List("defense", "interceptions"),
      List("interceptions"),
    ),
    "defPassesDefended" -> List(
      List("defense", "passes"),
      List("passes", "defense"),
      List("passes"),
    ),
    "defSacks" -> List(
      List("defense", "sacks"),
      List("sacks"),
    ),
    "defSoloTackles" -> List(
      List("defense", "solo", "tackles"),
      List("solo", "tackles"),
      List("tackles", "solo"),
      List("tackles"),
    ),
    "defTacklesForLoss" -> List(
      List("defense", "tackles", "loss"),
      List("defense", "loss"),
      List("tackles", "loss"),
      List("tackles"),
    ),
    "defTouchdowns" -> List(
      List("defense", "touchdown"),
      List("touchdown", "defense"),
      List("touchdown"),
    ),
    "defYards" -> List(
      List("defense", "yards"),
      List("yards", "defense"),
      List("yards"),
    ),
    "kickReturns" -> List(
      List("kick", "return"),
      List("return", "kick"),
      List("return"),
      List("kick"),
    ),
    "kickTouchdowns" -> List(
      List("kick", "touchdown"),
      List("touchdown", "kick"),
      List("touchdown"),
    ),
    "kickYards" -> List(
      List("kick", "yards"),
      List("yards", "kick"),
      List("yards"),
      List("kick"),
    ),
    "passAttempts" -> List(
      List("pass", "attempts"),
      List("attempts", "pass"),
      List("attempts"),
      List("pass"),
    ),
    "passCompletions" -> List(
      List("pass", "completions"),
      List("completions", "pass"),
      List("completions"),
      List("pass"),
    ),
    "passInterceptions" -> List(
      List("pass", "interceptions"),
      List("interceptions", "pass"),
      List("interceptions"),
      List("pass"),
    ),
    "passTouchdowns" -> List(
      List("pass", "touchdown"),
      List("touchdown", "pass"),
      List("touchdown"),
      List("pass"),
    ),
    "passYards" -> List(
      List("pass", "yards"),
      List("yards", "pass"),
      List("yards"),
      List("pass"),
    ),
    "puntReturns" -> List(
      List("punt", "return"),
      List("return", "punt"),
      List("return"),
      List("punt"),
    ),
    "puntYards" -> List(
      List("punt", "yards"),
      List("yards", "punt"),
      List("yards"),
      List("punt"),
    ),
    "recvReceptions" -> List(
      List("reception"),
    ),
    "recvTargeted" -> List(
      List("reception", "target"),
      List("target"),
    ),
    "recvTouchdowns" -> List(
      List("reception", "touchdown"),
      List("touchdown"),
      List("reception"),
    ),
    "recvYards" -> List(
      List("reception", "yards"),
      List("reception"),
      List("yards"),
    ),
    "rushCarries" -> List(
      List("rush", "carries"),
      List("carries"),
      List("rush"),
    ),
    "rushTouchdowns" -> List(
      List("rush", "touchdown"),
      List("touchdown"),
      List("rush"),
    ),
    "rushYards" -> List(
      List("rush", "yards"),
      List("yards"),
      List("rush"),
    ),
  )

  final val POSITION_PHRASES = Map(
    "LB" -> List(
      List("linebacker"),
      List("lineback"),
      List("line", "backer"),
      List("line", "back"),
    ),
    "QB" -> List(
      List("quarterback"),
      List("quarter"),
      List("quarter", "back"),
    ),
    "DE" -> List(
      List("defensive", "end"),
    ),
    "MLB" -> List(
      List("middle", "linebacker"),
      List("middle", "lineback"),
      List("middle", "line", "backer"),
      List("middle", "line", "back"),
    ),
    "OLB" -> List(
      List("offensive", "linebacker"),
      List("offensive", "lineback"),
      List("offensive", "line", "backer"),
      List("offensive", "line", "back"),
    ),
    "KR" -> List(
      List("kick", "returner"),
    ),
    "FB" -> List(
      List("fullback"),
      List("full", "back"),
      List("runningback"),
      List("running", "back"),
      List("fback"),
    ),
    "WR" -> List(
      List("receiver"),
      List("wide", "receiver"),
    ),
    "RB" -> List(
      List("halfback"),
      List("half", "back"),
      List("runningback"),
      List("running", "back"),
      List("hback"),
    ),
    "S" -> List(
      List("safety"),
      List("strong", "safety"),
      List("free", "safety"),
    ),
    "CB" -> List(
      List("cornerback"),
      List("corner", "back"),
      List("corner"),
    ),
  )

  final val STATES = Map(
    "AL" -> "Alabama",
    "KY" -> "Kentucky",
    "OH" -> "Ohio",
    "AK" -> "Alaska",
    "LA" -> "Louisiana",
    "OK" -> "Oklahoma",
    "AZ" -> "Arizona",
    "ME" -> "Maine",
    "OR" -> "Oregon",
    "AR" -> "Arkansas",
    "MD" -> "Maryland",
    "PA" -> "Pennsylvania",
    "AS" -> "American Samoa",
    "MA" -> "Massachusetts",
    "PR" -> "Puerto Rico",
    "CA" -> "California",
    "MI" -> "Michigan",
    "RI" -> "Rhode Island",
    "CO" -> "Colorado",
    "MN" -> "Minnesota",
    "SC" -> "South Carolina",
    "CT" -> "Connecticut",
    "MS" -> "Mississippi",
    "SD" -> "South Dakota",
    "DE" -> "Delaware",
    "MO" -> "Missouri",
    "TN" -> "Tennessee",
    "DC" -> "District of Columbia",
    "MT" -> "Montana",
    "TX" -> "Texas",
    "FL" -> "Florida",
    "NE" -> "Nebraska",
    "TT" -> "Trust Territories",
    "GA" -> "Georgia",
    "NV" -> "Nevada",
    "UT" -> "Utah",
    "GU" -> "Guam",
    "NH" -> "New Hampshire",
    "VT" -> "Vermont",
    "HI" -> "Hawaii",
    "NJ" -> "New Jersey",
    "VA" -> "Virginia",
    "ID" -> "Idaho",
    "NM" -> "New Mexico",
    "VI" -> "Virgin Islands",
    "IL" -> "Illinois",
    "NY" -> "New York",
    "WA" -> "Washington",
    "IN" -> "Indiana",
    "NC" -> "North Carolina",
    "WV" -> "West Virginia",
    "IA" -> "Iowa",
    "ND" -> "North Dakota",
    "WI" -> "Wisconsin",
    "KS" -> "Kansas",
    "CM" -> "Northern Mariana Islands",
    "WY" -> "Wyoming",
  )

}
