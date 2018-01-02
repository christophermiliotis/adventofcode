
object DayThreePartTwo {

  def sumOfValues(walkLimit: Int): Long = {
    val baseCoordinate = Coordinate(1, Point(0,0), Empty)
    val spiralMap = Map[Point, Int](baseCoordinate.point -> baseCoordinate.step)

    val walkHistory: Stream[(Coordinate, Map[Point, Int])] =
      Stream.iterate((baseCoordinate, spiralMap)){
        case (currentCoordinate, currentSpiral) =>
          walkNextStep(currentCoordinate, currentSpiral)
      }

    val (lastCoordinate, _) = walkHistory.dropWhile {
      case (coordinate, spiral) =>
        print(s"-> ${coordinate.step}")
        coordinate.step <= walkLimit
    }.head

    println(s"Found wanted step: [${lastCoordinate.step}]")
    lastCoordinate.step
  }

  def walkNextStep(currentCoordinate: Coordinate,
                   currentSpiral: Map[Point, Int]): (Coordinate, Map[Point, Int]) = {
    val allNeighbours = allPossibleNeighbors(currentCoordinate.point)
    val currentSum = sumOfNeighbors(allNeighbours, currentSpiral)
    val updatedMap = currentSpiral.updated(currentCoordinate.point, currentSum)

    val direction =
      currentCoordinate.direction.getNewDirection(currentCoordinate.point, updatedMap)

    val nextPoint = currentCoordinate.point.towards(direction)

    (Coordinate(currentSum, nextPoint, direction), updatedMap)
  }

  private def allPossibleNeighbors(currentPoint: Point) = {
    Seq(
      currentPoint.up,
      currentPoint.down,
      currentPoint.left,
      currentPoint.right,
      currentPoint.upperLeft,
      currentPoint.upperRight,
      currentPoint.downLeft,
      currentPoint.downRight
    )
  }

  private def sumOfNeighbors(neighbors: Seq[Point], spiral: Map[Point, Int]): Int = {
    val sum = neighbors.flatMap(spiral.get).sum
    if (sum == 0) 1 else sum
  }
}

case class Coordinate(step: Int, point: Point, direction: Direction)

case class Point(x: Int, y: Int) {

  def towards(direction: Direction): Point = {
    direction match {
      case Right => right
      case Up    => up
      case Left  => left
      case Down  => down
    }
  }

  def up = {
    Point(x, y + 1)
  }

  def down = {
    Point(x, y -1)
  }

  def left = {
    Point(x - 1, y)
  }

  def right = {
    Point(x + 1, y)
  }

  def upperLeft = {
    Point(x - 1, y + 1)
  }

  def upperRight = {
    Point(x + 1, y + 1)
  }

  def downLeft = {
    Point(x - 1, y - 1)
  }

  def downRight = {
    Point(x + 1, y  - 1)
  }
}

sealed trait Direction {
  def next: Direction
  def getNewDirection(currentPoint: Point, spiral: Map[Point, Int]): Direction = {
    if (spiral.get(lookAheadPoint(currentPoint)).isEmpty) next
    else this
  }

  def lookAheadPoint(currentPoint: Point): Point = currentPoint.towards(next)
}

case object Right extends Direction {
  override def next = Up
}

case object Up extends Direction {
  override def next = Left
}

case object Left extends Direction {
  override def next = Down
}

case object Down extends Direction {
  override def next = Right
}

case object Empty extends Direction {
  override def next = Right
}
