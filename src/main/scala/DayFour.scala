object DayFour {
  def numberOfValidPassphrases(input: Seq[String]): Int = {
    input.map { phrase =>
        val list = phrase.split(" ").toList
        if (list.distinct.size == list.size) 1 else 0
    }.sum
  }

  def numberOfValidPassphrasesWithoutAnagrams(input: Seq[String]): Int = {
    input.map { phrase =>
      val listOfWordsSortedByWordLength = phrase
        .split(" ")
        .toList
        .map(_.toCharArray.sorted.mkString)
        .sorted
      val listExcludingAnagrams = listOfWordsSortedByWordLength.foldLeft(List.empty[String]) {
        case (Nil, word)          => List(word)
        case (existingList, word) => if (existingList.last == word) existingList else existingList :+ word
      }
      if (listExcludingAnagrams.size == listOfWordsSortedByWordLength.size) 1 else 0
    }.sum
  }
}
