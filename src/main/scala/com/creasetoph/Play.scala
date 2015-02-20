package com.creasetoph

import com.creasetoph.Baseball._

object Play {

  def main(args: Array[String]): Unit = {
    val game = Game(Teams.teamFame,Teams.teamOps)
    val endGame = Game.play(game,probability)
    println(Printer.getPlays(endGame))
    println(Printer.boxScore(endGame))
    println(Printer.bothTeamStats(endGame))
  }

  def probability(p: Player): AtBat =
    if(math.random < .3) {
      val hit = math.random
      if(hit < .2) Hit(4)
      else if(hit < .3) Hit(3)
      else if(hit < .5) Hit(2)
      else Hit(1)
    } else Out
}
