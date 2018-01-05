
trait MazeDirection {
  def towards(offset: Int): MazeDirection = {
    if (offset < 0) MazeLeft
    else if (offset == 0) this
    else MazeRight
  }
}

case class Movement(offset: Int, index: Int, direction: MazeDirection, totalSteps: Int)

object DayFive {
  def numberOfRequiredSteps(input: Seq[Int]): Int = {
    val inputMap = input.zipWithIndex.map(e => (e._2, e._1)).toMap
    val baseMovement = Movement(0, 0, MazeRight, 0)

    val movementHistory: Stream[(Movement, Map[Int, Int])] = Stream.iterate((baseMovement,
      inputMap)) {
      case (currentMovement, currentMap) => jumpToNext(currentMovement, currentMap)
    }

    val (finalMovement, _) = movementHistory.dropWhile {
      case (movement, indexMap) if movement.direction == MazeRight =>
        movement.index <= input.length - 1
      case (movement, indexMap) if movement.direction == MazeLeft =>
        movement.index > 0
    }.head

    finalMovement.totalSteps
  }

  private def jumpToNext(currentMovement: Movement, currentMap: Map[Int, Int]): (Movement,
    Map[Int, Int]) = {
    val currentIndex = currentMovement.index
    val currentInstruction = currentMovement.offset
    val currentDirection = currentMovement.direction
    val updatedMap = currentMap.updated(currentIndex, currentInstruction + 1)
    if (currentInstruction == 0) {
      (Movement(1, currentMovement.index, currentDirection, currentMovement.totalSteps + 1),
        updatedMap)
    } else {
      val nextIndex = currentIndex + currentInstruction
      val nextInstruction: Int = updatedMap.getOrElse(nextIndex, currentInstruction)
      val nextDirection = currentDirection.towards(nextInstruction)
      val totalSteps = currentMovement.totalSteps + 1
      (Movement(nextInstruction, nextIndex, nextDirection, totalSteps), updatedMap)
    }
  }
}

case object MazeLeft extends MazeDirection

case object MazeRight extends MazeDirection

