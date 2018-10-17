package bowling

import org.scalatest.{FunSpec, Matchers}
import scala.util.Random;
import Score._

class BowlingSpec extends FunSpec with Matchers {

  val random = Random;

  describe("A Game") {
    describe("When it has 10 Frames") {
      it("should end") {
        assert( Game( List.fill(10)( Frame.randomFrame(1, random) ) ).end() )
      }
    }
    describe("When all rolls are 0") {
      it("should have a score of 0") {
        assert( Score.calcScore( (Frame(LastRoll(0, 0, None), 10, random) :: List.fill(9)( Frame(Regular(0, 0), 1, random) )) ) == 0 )
      }
    }
    describe("When all rolls are 5") {
      it("should have a score of 150") {
        assert( Score.calcScore( (Frame(LastRoll(5, 5, Some(5)), 10, random) :: List.fill(9)( Frame(Spare(5, 5), 1, random) )) ) == 150 )
      }
    }
    describe("When all rolls are 10") {
      it("should have a score of 300") {
        assert( Score.calcScore( (Frame(LastRoll(10, 10, Some(10)), 10, random) :: List.fill(9)( Frame(Strike(), 1, random) )) ) == 300 )
      }
    }
    describe("When all rolls are 1") {
      it("should have a score of 20") {
        assert( Score.calcScore( (Frame(LastRoll(1, 1, None), 10, random) :: List.fill(9)( Frame(Regular(1, 1), 1, random) )) ) == 20 )
      }
    }
    describe("When all rolls are 7 then 3") {
      it("should have a score of 170") {
        assert( Score.calcScore( (Frame(LastRoll(7, 3, Some(7)), 10, random) :: List.fill(9)( Frame(Spare(7, 3), 1, random) )) ) == 170 )
      }
    }
  }
  describe("A Roll") {
    it("should get between 0 and 10 pins down") {
      val pinsDown: Int = Roll.askRoll(11, random);
      assert( pinsDown <= 10 && pinsDown >= 0 )
    }
  }

}
