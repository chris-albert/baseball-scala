package com.creasetoph

import com.creasetoph.Teams.Team

object Baseball {

  def randomProbability = {math.random < .15}

  def main(args: Array[String]): Unit = {
    val game = Game(Teams.teamFame,Teams.teamOps)
    val endGame = Game.simulateGame(game,randomProbability)
    println(Game.boxScore(endGame))
  }

  case class Player(name: String,atBats: Int = 0,hits: Int = 0,money: Int = 0) {
    def gotHit = this.copy(atBats = atBats + 1,hits = hits + 1)
    def madeOut = this.copy(atBats = atBats + 1)
    def atBat(probability: => Boolean) = if(probability) gotHit else madeOut
    def battingAverage: Double = if(hits == 0) 0 else hits / atBats.toDouble
    def battingAverageFormat: String = battingAverage.formatted("%.3f")
  }
  
  case class GameTeam(team: Team,
                      currentPlayer: Int = 0,
                      innings: List[Inning] = List()) {
    def atBat(probability: => Boolean): GameTeam = {
      val newPlayers = team.players.zipWithIndex.map {
        case (player,index) =>
          if(index == currentPlayer)
            if(probability) player.gotHit else player.madeOut
          else player
      }
      val nextPlayer = if(currentPlayer == team.players.length) 0 else currentPlayer + 1
      this.copy(team = team.copy(players = newPlayers),currentPlayer = nextPlayer)
    }

    def runs: Int = innings.foldLeft(0)(_ + _.runs)
  }
  
  object GameTeam {
    def simulateAtBat(team: GameTeam,inning: Inning,
                      probability: => Boolean): (GameTeam,Inning) =
      team.atBat(probability) -> inning.atBat(probability)

    def simulateInning(team: GameTeam,
                       inning: Inning,
                       probability: => Boolean): GameTeam = {
      def bat(t: GameTeam,i: Inning): (GameTeam,Inning) =
        i match {
          case Inning(_,3,_) => t -> i
          case _ =>
            val a = simulateAtBat(t,i,probability)
            bat(a._1,a._2)
        }
      val (newTeam,newInning) = bat(team,inning)
      newTeam.copy(innings = newInning :: newTeam.innings)
    }
  }

  case class Inning(runs: Int = 0,
                    outs: Int = 0,
                    baseRunners: Int = 0) {
    def isBasesLoaded: Boolean = baseRunners == 3
    def isOver: Boolean = outs == 3
    def gotHit: Inning = this.copy(runs = runs + 1,outs,baseRunners)
    def madeOut: Inning = this.copy(runs,outs = outs + 1,baseRunners)
    def atBat(probability: => Boolean) = if(probability) gotHit else madeOut
  }

  case class Game(home: GameTeam,
                  away: GameTeam)

   object Game {

     def apply(home: Team,away: Team): Game =
       Game(GameTeam(home),GameTeam(away))

     def simulateGame(game: Game,probability: => Boolean): Game = {
       def playInning(inning: Int,game: Game): Game = {
         if(inning <= 9 || game.home.runs == game.away.runs) {
           val home = GameTeam.simulateInning(game.home,Inning(),probability)
           val away = GameTeam.simulateInning(game.away,Inning(),probability)
           playInning(inning + 1,Game(home,away))
         } else game
       }
       playInning(1,game)
     }

     def getBoxLine(team: String,innings: List[Int] = List(),
                    runs: String = "",hits: String = ""): String = {
       val i = innings.map(i => f"$i%2s").mkString("|")
       f"|$team%10s|$i|$runs%5s|$hits%5s|"
     }

     def boxScore(game: Game): String = {
       val header = getBoxLine("Team",List.range(1,game.away.innings.length + 1),"Runs","Hits")
       val border = (0 until header.length).map(_ => "-").mkString
       List(
         border,
         header,
         border,
         getBoxLine(game.away.team.name,game.away.innings.map(_.runs),game.away.runs.toString),
         getBoxLine(game.home.team.name,game.home.innings.map(_.runs),game.home.runs.toString),
         border
       ).mkString("\n")
     }

     def players(gameTeam: GameTeam): String = {
       gameTeam.team.players.map(player =>
         f"|${player.name}%10s|${player.hits}%5s|${player.atBats}%5s|"
       ).mkString("\n")
     }
   }

}


