package com.creasetoph

object Baseball {

  type AtBatProbability = Player => AtBat

  case class Team(name: String,players: List[Player])

  case class Player(name: String)

  trait AtBat
  case class Hit(bases: Int) extends AtBat
  case object Out extends AtBat

  case class AtBatOutcome(player: Player,bases: Bases,runs: Int = 0,
                          outs: Int = 0,numBases: Int = 0) {
    def gotHit: Boolean = numBases > 0
  }

  case class Bases(first: Option[Player] = None,
                   second: Option[Player] = None,
                   third: Option[Player] = None)

  object Bases {
    def processAtBat(atBat: AtBat,bases: Bases,batter: Player): AtBatOutcome =
      atBat match {
        case Hit(b) => advanceRunners(bases,b,batter)
        case Out    => AtBatOutcome(batter,bases,0,1,0)
      }

    def advanceRunners(bases: Bases,totalBases: Int, batter: Player): AtBatOutcome = {
      def run(b: Bases,numBases: Int,runs: Int,fromBox: Option[Player]): AtBatOutcome =
        if(numBases == 0) AtBatOutcome(batter,b,runs,0,totalBases)
        else run(Bases(fromBox,b.first,b.second),numBases - 1,if(b.third.isDefined) runs + 1 else runs,None)
      run(bases,totalBases,0,Some(batter))
    }
  }

  case class GameTeam(team: Team,lineup: List[Player],innings: List[Inning] = List()) {
    def runs: Int = innings.foldLeft(0)(_ + _.runs)
    def hits: Int = innings.foldLeft(0)(_ + _.hits)
    def batter: Player = lineup.head
    def next: GameTeam = this.copy(lineup = if(lineup.length == 1) team.players else lineup.tail)
  }

  object GameTeam {
    def apply(team: Team): GameTeam = GameTeam(team,team.players)
  }

  case class Inning(outcomes: List[AtBatOutcome] = List()) {
    def isOver: Boolean = outs == 3
    def runs: Int = outcomes.foldLeft(0)(_ + _.runs)
    def outs: Int = outcomes.foldLeft(0)(_ + _.outs)
    def hits: Int = outcomes.foldLeft(0){case (acc,out) => if(out.gotHit) acc + 1 else acc}
  }

  case class Game(home: GameTeam,away: GameTeam)

  object Game {
    def apply(home: Team,away: Team): Game = Game(GameTeam(home),GameTeam(away))

    def play(game: Game,probability: AtBatProbability): Game =
      playInnings(game.away,game.home,probability)

    def playInnings(away: GameTeam,home: GameTeam,probability: AtBatProbability): Game = {
      val inning = home.innings.length + 1
      if(inning >= 9) { //Maybe last inning
        if(away.runs > home.runs && inning > 9) Game(home,away) //Home team loses, game over
        else {
          val top = playHalfInning(Inning(), away, probability)
          if (top.runs < home.runs) Game(home, top) //Game is over, since top was played and away was losing
          else {
            val bottom = playHalfInning(Inning(), home, probability)
            if(bottom.runs > top.runs) Game(bottom,top)
            else playInnings(top,bottom, probability)
          }
        }
      } else //Not last inning, keep on playing
        playInnings(
          playHalfInning(Inning(),away,probability),
          playHalfInning(Inning(),home,probability),
          probability
        )
    }

    def playHalfInning(i: Inning,gameTeam: GameTeam,probability: AtBatProbability): GameTeam = {
      def bat(inning: Inning,bases: Bases,newTeam: GameTeam): GameTeam = {
        if(inning.isOver) newTeam.copy(innings = inning :: newTeam.innings)
        else {
          val batter = newTeam.batter
          val outcome = Bases.processAtBat(probability(batter),bases,batter)
          bat(inning.copy(outcomes = outcome :: inning.outcomes),outcome.bases,newTeam.next)
        }
      }
      bat(i,Bases(),gameTeam)
    }
  }
}
