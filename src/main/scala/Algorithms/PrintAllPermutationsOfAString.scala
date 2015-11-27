package Algorithms

class PrintAllPermutationsOfAString {

  def count(letters: String): Unit = {
    def count(letters: String, result: String): Unit = {
      if (letters.length == 0) {
        println(result)
      } else {
        letters.foreach { letter =>
          val filteredLetters = letters.filterNot(_ == letter)
          count(filteredLetters, result + letter)
        }
      }
    }
    count(letters, "")
  }

  def count2(letters: String): Unit = {
    letters.permutations.foreach(println)
  }

}
