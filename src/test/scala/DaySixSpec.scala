import org.scalatest.{FlatSpec, Matchers}

class DaySixSpec extends FlatSpec with Matchers {

  behavior of "The Memory Reallocation routine"

  it should "work as per the example" in {
    val input = Seq(0, 2, 7, 0)
    val actual = DaySix.numRedistributionCycles(input)
    actual._1 shouldBe 5
    actual._2 shouldBe 4
  }

  it should "allocate the memory blocks until it produces a blocks-in-bank config which has been seen before" in {
    val input = Seq(5,1,10,0,1,7,13,14,3,12,8,10,7,12,0,6)
    val redistributionCycles = DaySix.numRedistributionCycles(input)._1
    val cyclesSinceLastSeen = DaySix.numRedistributionCycles(input)._2
    redistributionCycles shouldBe 5042
    cyclesSinceLastSeen shouldBe 1086
  }
}
