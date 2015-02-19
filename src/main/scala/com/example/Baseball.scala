
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

  def randomProbability = {math.random < .15}

  def main(args: Array[String]): Unit = {
    val game = BaseballGame("Fame",teamFame,"Ops",teamOps)
    val endGame = BaseballGame.simulateGame(game,randomProbability)
    println(BaseballGame.boxScore(endGame))
  }

  case class BaseballPlayer(name: String,atBats: Int = 0,hits: Int = 0,money: Int = 0) {
    def gotHit = this.copy(atBats = atBats + 1,hits = hits + 1)
    def madeOut = this.copy(atBats = atBats + 1)
    def atBat(probability: => Boolean) = if(probability) gotHit else madeOut
    def battingAverage: Double = if(hits == 0) 0 else hits / atBats.toDouble
    def battingAverageFormat: String = battingAverage.formatted("%.3f")
  }
  
  case class BaseballGameTeam(name: String,
                              players: List[BaseballPlayer],
                              currentPlayer: Int = 0,
                              innings: List[BaseballInning] = List()) {
    def atBat(probability: => Boolean): BaseballGameTeam = {
      val newPlayers = players.zipWithIndex.map {
        case (player,index) =>
          if(index == currentPlayer)
            if(probability) player.gotHit else player.madeOut
          else player
      }
      this.copy(players = newPlayers)
    }

    def runs: Int = innings.foldLeft(0)(_ + _.runs)
  }
  
  object BaseballGameTeam {
    def simulateAtBat(team: BaseballGameTeam,inning: BaseballInning,
                      probability: => Boolean): (BaseballGameTeam,BaseballInning) =
      team.atBat(probability) -> inning.atBat(probability)

    def simulateInning(team: BaseballGameTeam,
                       inning: BaseballInning,
                       probability: => Boolean): BaseballGameTeam = {
      def bat(t: BaseballGameTeam,i: BaseballInning): (BaseballGameTeam,BaseballInning) =
        i match {
          case BaseballInning(_,3,_) => t -> i
          case _ =>
            val a = simulateAtBat(t,i,probability)
            bat(a._1,a._2)
        }
      val (newTeam,newInning) = bat(team,inning)
      newTeam.copy(innings = newInning :: newTeam.innings)
    }
  }

  case class BaseballInning(runs: Int = 0,
                            outs: Int = 0,
                            baseRunners: Int = 0) {
    def isBasesLoaded: Boolean = baseRunners == 3
    def isOver: Boolean = outs == 3
    def gotHit: BaseballInning = this.copy(runs = runs + 1,outs,baseRunners)
    def madeOut: BaseballInning = this.copy(runs,outs = outs + 1,baseRunners)
    def atBat(probability: => Boolean) = if(probability) gotHit else madeOut
  }

  case class BaseballGame(home: BaseballGameTeam,
                          away: BaseballGameTeam)

   object BaseballGame {

     def apply(homeName: String,home: List[BaseballPlayer],
               awayName: String,away: List[BaseballPlayer]): BaseballGame =
       BaseballGame(BaseballGameTeam(homeName,home),BaseballGameTeam(awayName,away))

     def simulateGame(game: BaseballGame,probability: => Boolean): BaseballGame = {
       def playInning(inning: Int,game: BaseballGame): BaseballGame = {
         if(inning <= 9 || game.home.runs == game.away.runs) {
           val home = BaseballGameTeam.simulateInning(game.home,BaseballInning(),probability)
           val away = BaseballGameTeam.simulateInning(game.away,BaseballInning(),probability)
           playInning(inning + 1,BaseballGame(home,away))
         } else game
       }
       playInning(1,game)
     }

     def getBoxLine(team: String,innings: List[Int] = List(),
                    runs: String = "",hits: String = ""): String = {
       val i = innings.map(i => f"$i%2s").mkString("|")
       f"|$team%10s|$i|$runs%5s|$hits%5s|"
     }

     def boxScore(game: BaseballGame): String = {
       val header = getBoxLine("Team",List.range(1,game.away.innings.length + 1),"Runs","Hits")
       val border = (0 until header.length).map(_ => "-").mkString
       List(
         border,
         header,
         border,
         getBoxLine(game.away.name,game.away.innings.map(_.runs),game.away.runs.toString),
         getBoxLine(game.home.name,game.home.innings.map(_.runs),game.home.runs.toString),
         border
       ).mkString("\n")
     }
   }

}


