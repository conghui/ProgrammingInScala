import scala.collection.mutable

object Chap17 {
  def countWords(text: String) = {
    val counts = mutable.Map.empty[String, Int]
    for (rawWord <- text.split("[ ,!.]+")) {
      val word = rawWord.toLowerCase
      val oldCount =
        if (counts.contains(word)) counts(word)
        else 0
      counts += (word -> (oldCount + 1))
    }

    counts
  }

  countWords("See Spot run! Run, Spot. Run!")
}
