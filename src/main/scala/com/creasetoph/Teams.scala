package com.creasetoph

import com.creasetoph.Baseball.Player

object Teams {

  case class Team(name: String,players: List[Player])

  val teamFame = Team("FAME",List(
    Player("Chris"),
    Player("Tony"),
    Player("Ryan"),
    Player("Josh"),
    Player("Liangnan"),
    Player("Emma"),
    Player("Steve")
  ))

  val teamOps = Team("OPS",List(
    Player("Ron"),
    Player("Harper"),
    Player("Shantanu"),
    Player("David"),
    Player("Rommel"),
    Player("Roopa")
  ))
}
