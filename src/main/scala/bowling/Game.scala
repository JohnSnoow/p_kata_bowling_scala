package bowling;

case class Game( listFrames: List[Frame] = List[Frame]() ) {

  def end(): Boolean = {
    listFrames.length == 10;
  }
}
