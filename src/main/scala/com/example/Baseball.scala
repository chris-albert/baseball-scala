import scala.util.Random

object Baseball {

  val teamFame = List(
    BaseballPlayer("Chris"),
    BaseballPlayer("Tony"),
    BaseballPlayer("Ryan"),
    BaseballPlayer("Josh"),
    BaseballPlayer("Liangnan"),
    BaseballPlayer("Emma"),
    BaseballPlayer("Steve")
  )

  val teamOps = List(
    BaseballPlayer("Ron"),
    BaseballPlayer("Harper"),
    BaseballPlayer("Shantanu"),
    BaseballPlayer("David"),
    BaseballPlayer("Rommel"),
    BaseballPlayer("Roopa")
  )

  def randomProbability = {Random.nextBoolean()}

  def main(args: Array[String]): Unit = {
    val game = BaseballGame("Fame",teamFame,"Ops",teamOps)
    val endGame = BaseballGame.simulateGame(game,randomProbability)
    println(BaseballGame.boxScore(endGame))
  }

  case class BaseballPlayer(name: String,atBats: Int = 0,hits: Int = 0,money: Int = 0) {
    def gotHit = this.copy(atBats = atBats + 1,hits = hits + 1)
    def madeOut = this.copy(atBats = atBats + 1)
    def battingAverage: Double = if(hits == 0) 0 else hits / atBats.toDouble
    def battingAverageFormat: String = battingAverage.formatted("%.3f")
  }
  
  case class BaseballGameTeam(name: String,
                              players: List[BaseballPlayer],
                              currentPlayer: Int = 0,
                              baseRunners: Int = 0,
                              runs: Int = 0,
                              outs: Int = 0) 
  
  object BaseballGameTeam {
    def simulateAtBat(team: BaseballGameTeam,gotHit: Boolean): BaseballGameTeam = {
      val newPlayers = team.players.zipWithIndex.map {
        case (player,index) =>
          if(index == team.currentPlayer) 
            if(gotHit) player.gotHit else player.madeOut
          else player
      }
      val nextPlayer = 
        if(team.currentPlayer < team.players.size) team.currentPlayer + 1
        else 0
      val (newBaseRunners,newRuns,newOuts) = 
        if(gotHit && team.baseRunners == 3) (team.baseRunners,team.runs + 1,team.outs)
        else if(gotHit) (team.baseRunners + 1,team.runs,team.outs)
        else (team.baseRunners,team.runs,team.outs + 1)
      BaseballGameTeam(team.name,newPlayers,nextPlayer,newBaseRunners,newRuns,newOuts)
    }
    def simulateInning(team: BaseballGameTeam,probability: => Boolean): BaseballGameTeam = {
      val newInningTeam = team.copy(outs = 0,baseRunners = 0)
      def bat(t: BaseballGameTeam): BaseballGameTeam =
        if(t.outs < 3) bat(simulateAtBat(t,probability))
        else t
      bat(newInningTeam)
    }
  }

  case class BaseballInning(runs: Int = 0,
                            outs: Int = 0,
                            baseRunners: Int = 0)

  case class BaseballGame(home: BaseballGameTeam,
                          away: BaseballGameTeam) {
    override def toString = List(
      s"Home [${home.name}], runs [${home.runs}]",
      s"Away [${away.name}], runs [${away.runs}]"
    ).mkString("\n")
  }

   object BaseballGame {

     def apply(homeName: String,home: List[BaseballPlayer],
               awayName: String,away: List[BaseballPlayer]): BaseballGame =
       BaseballGame(BaseballGameTeam(homeName,home),BaseballGameTeam(awayName,away))

     def simulateGame(game: BaseballGame,probability: => Boolean): BaseballGame = {
       def playInning(inning: Int,game: BaseballGame): BaseballGame = {
         if(inning <= 9 || game.home.runs == game.away.runs) {
           playInning(inning + 1,
             BaseballGame(BaseballGameTeam.simulateInning(game.home, probability),
               BaseballGameTeam.simulateInning(game.away, probability)))
         } else game
       }
       playInning(1,game)
     }

     def getBoxLine(team: String,innings: List[Int] = List(),
                    runs: String = "",hits: String = ""): String = {
       val i = List.range(0,innings.length).map(i => f"$i%2s").mkString("|")
       f"|$team%10s|$i|$runs%5s|$hits%5s|"
     }

     def boxScore(game: BaseballGame): String = {
       val header = getBoxLine("Team",List.range(0,10),"Runs","Hits")
       val border = (0 until header.length).map(_ => "-").mkString
       List(
         border,
         header,
         border,
         getBoxLine(game.away.name),
         getBoxLine(game.home.name),
         border
       ).mkString("\n")
     }
   }

}


