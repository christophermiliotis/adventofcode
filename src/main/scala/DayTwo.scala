
object DayTwo {

  def calculateChecksum(input: Seq[String]): Long = {
    val inputAsIntSeq = toArrayOfIntSeq(input)
    val diffs = inputAsIntSeq.map { row => row.max - row.min }
    diffs.sum
  }

  def calculateChecksumWithDivision(input: Seq[String]): Long = {
    val inputAsIntSeq = toArrayOfIntSeq(input)
    val evenlyDivisibleCombinations = inputAsIntSeq.flatMap { row =>
      val combinationsOfTwo = row.combinations(2)
      combinationsOfTwo.find { combo =>
        val sorted = combo.sorted
        sorted.last % sorted.head == 0
      }
    }.map(_.sorted)

    evenlyDivisibleCombinations
      .map { combo => combo.last / combo.head }
      .sum
  }

  private def toArrayOfIntSeq(input: Seq[String]): Seq[Array[Int]] = {
    input.map(row => row.split("\t").map(_.toInt))
  }
}
