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
    def next: GameTeam = this.copy(lineup = if(lineup.isEmpty) team.players else lineup.tail)
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

    def play(game: Game,probability: AtBatProbability): Game = {
      logAtBatHeaders()
      playInnings(game.away,game.home,probability)
    }

    def playInnings(away: GameTeam,home: GameTeam,probability: AtBatProbability): Game = {
      if(home.innings.length >= 9 && home.runs != away.runs) //Game is over
        Game(home,away)
      else if(away.innings.length >= 9 && home.runs > away.runs) //Game is over
        Game(home,away)
      else
        playInnings(
          playHalfInning(Inning(),away,probability),
          playHalfInning(Inning(),home,probability),
          probability
        )
    }

    def playHalfInning(i: Inning,gameTeam: GameTeam,probability: AtBatProbability): GameTeam = {
      def bat(inning: Inning,bases: Bases,newTeam: GameTeam): GameTeam = {
        if(inning.isOver) gameTeam.copy(innings = inning :: gameTeam.innings)
        else {
          val batter = gameTeam.batter
          val outcome = Bases.processAtBat(probability(batter),bases,batter)
          logAtBat(gameTeam,outcome)
          bat(inning.copy(outcomes = outcome :: inning.outcomes),outcome.bases,newTeam.next)
        }
      }
      bat(i,Bases(),gameTeam)
    }
  }

  val shouldLog = true

  def logAtBatHeaders() =
    if(shouldLog) println(Printer.atBatHeader)

  def logAtBat(gameTeam: GameTeam,outcome: AtBatOutcome): Unit = {
    if(shouldLog) println(Printer.atBat(gameTeam,outcome))
  }
}
