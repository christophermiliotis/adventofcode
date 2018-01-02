import org.scalatest.{FlatSpec, Matchers}

class DayThreeSpec extends FlatSpec with Matchers {

  private val maxNumbersInSpiral = 4100

  behavior of "Day Three"

  it should "calculate the steps required to reach the access port of the memory system" in {
    DayThree.numOfSteps(1, maxNumbersInSpiral) shouldBe 0
    DayThree.numOfSteps(12, maxNumbersInSpiral) shouldBe 3
    DayThree.numOfSteps(23, maxNumbersInSpiral) shouldBe 2
    DayThree.numOfSteps(1024, maxNumbersInSpiral) shouldBe 31
    DayThree.numOfSteps(9, maxNumbersInSpiral) shouldBe 1
    DayThree.numOfSteps(25, maxNumbersInSpiral) shouldBe 4
    DayThree.numOfSteps(49, maxNumbersInSpiral) shouldBe 6
  }

  it should "identify the perfect squares" in {
    DayThree.isPerfectSquare(3.0) shouldBe true
    DayThree.isPerfectSquare(4.1) shouldBe false
  }

  it should "calculate the steps for each number in a certain spiral" in {
    val numbers = 10 to 25
    val minAndMaxDistance = (2, 4)
    val expected = Map(
      10 -> 3, 11 -> 2, 12 -> 3, 13 -> 4,
      14 -> 3, 15 -> 2, 16 -> 3, 17 -> 4,
      18 -> 3, 19 -> 2, 20 -> 3, 21 -> 4,
      22 -> 3, 23 -> 2, 24 -> 3, 25 -> 4
    )
    DayThree.calculateStepsForEachNumberInSpiral(numbers, minAndMaxDistance) shouldBe expected
  }

  it should "calculate the steps for the puzzle input" in {
    DayThree.numOfSteps(325489, 400000) shouldBe 552
  }

  behavior of "Part Two"

  it should "calculate the next larger value" in {
    DayThreePartTwo.sumOfValues(325489) shouldBe 330785
  }
}
