package bowling;

import scala.util.Random;

abstract class Roll()

case class Regular (firstRoll: Int, secondRoll: Int) extends Roll()

case class Spare (firstRoll: Int, secondRoll: Int) extends Roll()

case class Strike () extends Roll()

case class LastRoll (firstRoll: Int, secondRoll: Int, thirdRoll: Option[Int]) extends Roll()

object Roll {

  def askRoll(pinsLeft: Int = 11, random: Random): Int = {
    random.nextInt(pinsLeft)
  }

  def buildRoll(numberRoll: Int, random: Random): Roll = {
    val newPinsDown = Roll.askRoll(11, random);

    if (numberRoll == 2) { // Not the last Frame
      if (newPinsDown == 10) {
        Strike();
      } else {
        val secondRollResult = Roll.askRoll(11 - newPinsDown, random);
        if (newPinsDown + secondRollResult == 10) {
         Spare(newPinsDown, secondRollResult);
       } else {
         Regular(newPinsDown, secondRollResult);
       }
      }
    } else { // the last Frame
      if (newPinsDown == 10) { // strike
        val secondRollResult = Roll.askRoll(11, random);
        if (secondRollResult == 10) { // second Strike
          val thirdRollResult = Roll.askRoll(11, random);
          LastRoll(newPinsDown, secondRollResult, Some(thirdRollResult))
        } else { // not a strike
          val thirdRollResult = Roll.askRoll(11 - secondRollResult, random);
          LastRoll(newPinsDown, secondRollResult, Some(thirdRollResult))
        }
      } else { // not a strike
        val secondRollResult = Roll.askRoll(11 - newPinsDown, random);
        if (newPinsDown + secondRollResult == 10) { // spare
          val thirdRollResult = Roll.askRoll(11, random);
          LastRoll(newPinsDown, secondRollResult, Some(thirdRollResult))
        } else {
          LastRoll(newPinsDown, secondRollResult, None)
        }
      }
    }
  }

}
