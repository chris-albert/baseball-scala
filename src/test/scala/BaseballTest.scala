import com.creasetoph.Baseball
import com.creasetoph.Baseball._
import org.scalatest.{Matchers, FlatSpec}

class BaseballTest extends FlatSpec with Matchers {

  //Outs
  val PopOut    = Baseball.Out
  val FlyOut    = Baseball.Out
  val LineOut   = Baseball.Out
  val GroundOut = Baseball.Out
  //Hits
  val Single  = Hit(1)
  val Double  = Hit(2)
  val Triple  = Hit(3)
  val HomeRun = Hit(4)

  val basesLoaded = Bases(Some(Player("first")),Some(Player("second")),Some(Player("third")))

  "Bases" should "Do nothing if an out" in {
    Bases.processAtBat(PopOut,Bases(),Player("batter")) shouldBe
      AtBatOutcome(Player("batter"),Bases(),0,1)
  }
  "Bases" should "Be cleared if bomb, and no runners on " in {
    Bases.processAtBat(HomeRun,Bases(),Player("batter")) shouldBe
      AtBatOutcome(Player("batter"),Bases(),1)
  }
  "Bases" should "Be cleared if bomb, and grand slam" in {
    Bases.processAtBat(HomeRun,basesLoaded,Player("batter")) shouldBe
      AtBatOutcome(Player("batter"),Bases(),4)
  }
  "Bases" should "Score run and be on first if single" in {
    Bases.processAtBat(Single,Bases(third = Some(Player("2"))),Player("batter")) shouldBe
      AtBatOutcome(Player("batter"),Bases(first = Some(Player("batter"))),1)
  }

  "Game" should "Not play bottom of last inning if home is winning" in {
//    val
  }
}
