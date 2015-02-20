package com.creasetoph

import com.creasetoph.Baseball._

object Printer {
  def getBoxLine(team: String,innings: List[String] = List(),
                 runs: String = "",hits: String = ""): String = {
    val i = innings.map(i => f"$i%2s").mkString("|")
    f"|$team%10s|$i|$runs%5s|$hits%5s|"
  }

  def boxScore(game: Game): String = {
    val inningHeader = List.range(1,game.away.innings.length + 1).map(_.toString)
    val header = getBoxLine("Team",inningHeader,"Runs","Hits")
    val border = (0 until header.length).map(_ => "-").mkString
    val innings = game.away.innings.length
    val awayInnings = game.away.innings.reverse.map(_.runs.toString)
    val maybeLastInning =
      if(game.away.innings.length > game.home.innings.length) List("x") else List()
    val homeInnings = game.home.innings.reverse.map(_.runs.toString) ++ maybeLastInning
    List("Box",border,header,border,
      getBoxLine(game.away.team.name,awayInnings,game.away.runs.toString,game.away.hits.toString),
      getBoxLine(game.home.team.name,homeInnings,game.home.runs.toString,game.home.hits.toString),
      border
    ).mkString("\n")
  }

  def getDivider(s: String): String = (0 until s.length).map(_ => "-").mkString

  def getPlays(team: GameTeam): String =
    List(
      "Plays",
      atBatHeader,
      eachInning(team.team.name,1,team.innings.reverse,List())
    ).mkString("\n")

  def getPlays(game: Game): String = {
    val header = atBatHeader
    val divider = getDivider(header)
    val plays = game.away.innings.reverse.zip(game.home.innings.reverse).zipWithIndex.map {
        case ((away,home),index) =>
          getInningPlays(game.away.team.name,index + 1,away) + "\n" +
            getInningPlays(game.home.team.name,index + 1,home)
      }
    List(
      "Plays",
      divider,
      header,
      divider,
      plays.mkString("\n"),
      divider
    ).mkString("\n")
  }

  def eachInning(name: String,numInning: Int,innings: List[Inning],acc: List[String]): String = {
    innings.headOption match {
      case Some(i) =>
        eachInning(name,numInning + 1,innings.tail,getInningPlays(name,numInning,i) :: acc)
      case None => acc.reverse.mkString("\n")
    }
  }

  def bothTeamStats(game: Game): String =
    List(
      "Home",
      teamStats(game.home),
      "Away",
      teamStats(game.away)
    ).mkString("\n")

  def teamStats(team: GameTeam): String = {
    val header = playerStatLine("Name","Hits","Bases","Outs","RBI's")
    val divider = getDivider(header)
    val stats = team.innings.map(_.outcomes).flatten.groupBy(_.player).map {
      case (player,outcomes) =>
        val hits = outcomes.foldLeft(0){case(i,o) => i + (if(o.gotHit) 1 else 0)}
        val bases = outcomes.foldLeft(0)(_ + _.numBases)
        val outs = outcomes.foldLeft(0)(_ + _.outs)
        val rbis = outcomes.foldLeft(0)(_ + _.runs)
        playerStatLine(player.name,hits.toString,bases.toString,outs.toString,rbis.toString)
    }
    List(divider,header,divider,stats.mkString("\n"),divider).mkString("\n")
  }

  def playerStatLine(name: String,hits: String,bases: String,outs: String,rbis: String): String =
    f"|$name%10s|$hits%5s|$bases%5s|$outs%5s|$rbis%5s|"

  def getInningPlays(team: String,numInning: Int,inning: Inning): String =
    inning.outcomes.reverse.map(outcome =>
      atBatLine(team,numInning.toString,outcome.player.name,
        outcome.numBases.toString,outcome.outs.toString,outcome.runs.toString)
    ).mkString("\n")

  def atBat(gameTeam: GameTeam,outcome: AtBatOutcome): String =
    atBatLine(gameTeam.team.name,(gameTeam.innings.length + 1).toString,outcome.player.name,
      outcome.numBases.toString,outcome.outs.toString,outcome.runs.toString)

  def atBatHeader: String =
    atBatLine("Team","Inning","Player","Bases","Outs","Runs")

  def atBatLine(team: String,inning: String,player: String,bases: String,outs: String,runs: String): String =
    f"|$team%10s|$inning%6s|$player%10s|$bases%5s|$outs%5s|$runs%5s|"
}
