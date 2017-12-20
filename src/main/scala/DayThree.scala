import scala.collection.mutable.ListBuffer

object DayThree {
  def numOfSteps(input: Int, maxNumbersInMemory: Int): Int = {
    if (input == 1) 0
    else {
      val oddNumbers = allOddNumbers(maxNumbersInMemory)
      val spiralLevels = oddNumbers.zipWithIndex.map(e => (e._1, e._2 + 1)).toMap
      val sqrtOfInput = Math.sqrt(input)
      val sqrtOfInputAsInt = sqrtOfInput.toInt
      if (isPerfectSquare(sqrtOfInput) && isOddNumber(sqrtOfInputAsInt)) {
        if (sqrtOfInput == 3) 1
        else spiralLevels(sqrtOfInputAsInt) * 2
      } else {
        // find between which two spirals the number belongs
        val currentSpiralLevel = findCurrentSpiralLevel(sqrtOfInputAsInt, spiralLevels)

        val numbersInTheSpiral = generateSpiral(sqrtOfInputAsInt)

        val minAndMaxDistance = minAndMaxDistancesForSpiral(currentSpiralLevel)
        val numbersWithStepsRequiredInThisSpiral = calculateStepsForEachNumberInSpiral(numbersInTheSpiral, minAndMaxDistance(currentSpiralLevel))
        numbersWithStepsRequiredInThisSpiral(input)
      }
    }
  }

  private def allOddNumbers(maxNumbersInMemory: Int) = {
    for {
      i <- 3 to maxNumbersInMemory
      if i % 2 == 1
    } yield i
  }

  private def isOddNumber(number: Int) = {
    number % 2 == 1
  }

  private def findCurrentSpiralLevel(number: Int, spiralLevels: Map[Int, Int]) = {
    if (isOddNumber(number)) {
      spiralLevels(number + 2)
    } else {
      spiralLevels(number + 1)
    }
  }

  private def generateSpiral(number: Int): Seq[Int] = {
    val (min, max) = if (isOddNumber(number)) {
      (number, number + 2)
    } else {
      (number - 1, number + 1)
    }
    ((Math.pow(min, 2).toInt + 1) to Math.pow(max, 2).toInt).toList
  }

  private def minAndMaxDistancesForSpiral(spiralLevel: Int): Map[Int, (Int, Int)] = {
    if (spiralLevel == 1) Map(1 -> (1, 1))
    else {
      Map(spiralLevel -> (spiralLevel, spiralLevel * 2))
    }
  }

  def isPerfectSquare(sqrt: Double) = {
    val x = sqrt.toInt
    Math.pow(sqrt, 2) == Math.pow(x, 2)
  }

  def calculateStepsForEachNumberInSpiral(numbers: Seq[Int], minAndMaxDistance: (Int, Int)): Map[Int, Int] = {
    // split the numbers in 4 groups, each groups represents a side of the spiral
    val numbersOnEachSide: Iterator[Seq[Int]] = numbers.grouped(numbers.size / 4)

    // calculate the steps for each number in the spiral
    val numberWithDistancePairs: Iterator[Seq[(Int, Int)]] = numbersOnEachSide.map(calculateNumberOfSteps(minAndMaxDistance, _))

    var stepsPerNumber: Map[Int, Int] = Map()
    numberWithDistancePairs.foreach { side =>
      val numberWithDistancePairsMap = side.toMap
      stepsPerNumber = numberWithDistancePairsMap ++ stepsPerNumber
    }
    stepsPerNumber
  }

  def calculateNumberOfSteps(minAndMaxDistance: (Int, Int), spiralSide: Seq[Int]): ListBuffer[(Int, Int)] = {
    val numbersAndStepsPairs = ListBuffer[(Int, Int)]()
    var currentDistance = 1
    var reachedMiddle = false
    for (elem <- spiralSide) {
      if (currentDistance < minAndMaxDistance._1 && !reachedMiddle) {
        numbersAndStepsPairs += ((elem, minAndMaxDistance._2 - currentDistance))
        currentDistance += 1
      } else if (currentDistance == minAndMaxDistance._1 && !reachedMiddle) {
        numbersAndStepsPairs += ((elem, minAndMaxDistance._2 - currentDistance))
        currentDistance -= 1
        reachedMiddle = true
      } else {
        numbersAndStepsPairs += ((elem, minAndMaxDistance._2 - currentDistance))
        currentDistance -= 1
      }
    }
    numbersAndStepsPairs
  }
}
