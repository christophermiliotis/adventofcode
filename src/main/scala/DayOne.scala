
object DayOne {

  def solveCaptcha(input: Seq[Int], zipFunction: Seq[Int] => Seq[Int]): Int = {
    if (input.isEmpty) 0
    else {
      input
        .zip(zipFunction(input))
        .filter(pair => pair._1 == pair._2)
        .map(_._1)
        .sum
    }
  }

  def solveCaptchaPartOneAlternative(input: Seq[Int]): Int = {
    if (input.isEmpty) 0 else {
      val pairs = input.sliding(2).toList ++ List(Seq(input.head, input.last))
      pairs
        .filter(l => l.head == l.last)
        .map(_.head)
        .sum
    }
  }

  def zipFunctionPartOne(input: Seq[Int]): Seq[Int] = {
    input.tail ++ input.take(1)
  }

  def zipFunctionPartTwo(input: Seq[Int]): Seq[Int] = {
    val window = input.size / 2
    input.takeRight(window) ++ input.take(window)
  }

  def zipFunctionPartTwoAlternative(input: Seq[Int]): Seq[Int] = {
    val window = input.length / 2
    val splitList = input.splitAt(window)
    splitList._2 ++ splitList._1
  }
}
