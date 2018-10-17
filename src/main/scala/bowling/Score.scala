package bowling;

import scala.annotation.tailrec;

object Score {

  @tailrec
  def calcScore(listFrames: List[Frame], listScore: List[Int] = List[Int](), index: Int = 0): Int = {
    val newListFrames: List[Frame] = if (index == 0) listFrames.reverse else listFrames;
    if (index >= 10 || listFrames.length == 0) listScore.sum
    else {
      newListFrames.head.roll match {
        case Regular(firstRoll, secondRoll) =>
          val frameScore = firstRoll + secondRoll;
          calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
        case Spare(firstRoll, secondRoll) =>
          newListFrames(1).roll match {
            case Regular(firstRollBis, secondRollBis) =>
              val frameScore = firstRoll + secondRoll + firstRollBis;
              calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
            case Spare(firstRollBis, secondRollBis) =>
              val frameScore = firstRoll + secondRoll + firstRollBis;
              calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
            case Strike() =>
              val frameScore = firstRoll + secondRoll + 10;
              calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
            case LastRoll(firstRollBis, secondRollBis, thirdRollBis) =>
              val frameScore = firstRoll + secondRoll + firstRollBis;
              calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
          }

        case Strike() =>
          newListFrames(1).roll match {
            case Regular(firstRoll, secondRoll) =>
              val frameScore = 10 + firstRoll + secondRoll;
              calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
            case Spare(firstRoll, secondRoll) =>
              val frameScore = 10 + firstRoll + secondRoll;
              calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
            case Strike() =>
              newListFrames(2).roll match {
                case Strike() =>
                  val frameScore = 30;
                  calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
                case Regular(firstRoll, secondRoll) =>
                  val frameScore = 20 + firstRoll;
                  calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
                case Spare(firstRoll, secondRoll) =>
                  val frameScore = 20 + firstRoll;
                  calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
                case LastRoll(firstRoll, secondRoll, thirdRoll) =>
                  val frameScore = 20 + firstRoll;
                  calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
              }
            case LastRoll(firstRoll, secondRoll, thirdRoll) =>
              val frameScore = 10 + firstRoll + secondRoll;
              calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
          }
        case LastRoll(firstRoll, secondRoll, thirdRoll) =>
          val frameScore = firstRoll + secondRoll + thirdRoll.getOrElse(0);
          calcScore(newListFrames.drop(1), frameScore :: listScore, index+1 )
      }

    }
  }
}
