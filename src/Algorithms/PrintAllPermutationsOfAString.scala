package Algorithms

class PrintAllPermutationsOfAString {

  def count(letters: String): Unit = {
    def count(letters: String, result: String): Unit = {
      if (letters.length == 0) {
        println(result)
      } else {
        letters.foreach((x) => {
          val filteredLetters = letters.filter((letter) => {
            !letter.equals(x)
          })
          count(filteredLetters, result + x)
        })
      }
    }
    count(letters, "")
  }


}
