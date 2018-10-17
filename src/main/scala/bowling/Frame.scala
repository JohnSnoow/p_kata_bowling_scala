package bowling;

import scala.util.Random;
import scala.collection.immutable.List;

case class Frame(roll: Roll, numberFrame: Int, random: Random )

object Frame {

  def randomFrame(numeroFrame: Int, random: Random): Frame = {
    if (numeroFrame == 10) {
      Frame(Roll.buildRoll(3, random), numeroFrame, random) ;
    } else {
      Frame(Roll.buildRoll(2, random), numeroFrame, random) ;
    }
  }
}
