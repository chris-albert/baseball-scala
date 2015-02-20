package com.creasetoph

import com.creasetoph.Baseball._

object Printer {
  def getBoxLine(team: String,innings: List[Int] = List(),
                 runs: String = "",hits: String = ""): String = {
    val i = innings.map(i => f"$i%2s").mkString("|")
    f"|$team%10s|$i|$runs%5s|$hits%5s|"
  }

  def boxScore(game: Game): String  = {
    val header = getBoxLine("Team",List.range(1,game.away.innings.length + 1),"Runs","Hits")
    val border = (0 until header.length).map(_ => "-").mkString
    List(border,header,border,
      getBoxLine(game.away.team.name,game.away.innings.reverse.map(_.runs),game.away.runs.toString,game.away.hits.toString),
      getBoxLine(game.home.team.name,game.home.innings.reverse.map(_.runs),game.home.runs.toString,game.home.hits.toString),
      border
    ).mkString("\n")
  }

  def atBat(gameTeam: GameTeam,outcome: AtBatOutcome): String =
    atBatLine(gameTeam.team.name,(gameTeam.innings.length + 1).toString,outcome.player.name,
      outcome.numBases.toString,outcome.outs.toString,outcome.runs.toString)

  def atBatHeader: String =
    atBatLine("Team","Inning","Player","Bases","Outs","Runs")

  def atBatLine(team: String,inning: String,player: String,bases: String,outs: String,runs: String): String =
    f"|$team%10s|$inning%6s|$player%10s|$bases%5s|$outs%5s|$runs%5s|"
}
