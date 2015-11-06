// n^3
implicit class NewString(string: String) {
  def getAllSubstrings: List[String] = {
    string
      .inits
      .flatMap(_.tails)
      .toList
      .filterNot(_.isEmpty)
  }

  def isPalindrome: Boolean = string.toList match {
    case Nil => true
    case head :: Nil => true
    case head :: tail if head != tail.last => false
    case head :: tail => tail.init.mkString.isPalindrome
  }
}
def findLongestPalindrome(string: String) = {
  string.getAllSubstrings.filter(_.isPalindrome).sortBy(_.length).last
}

// n^2
def findLongestPalindrome2(string: String) = {
  var longestStart = 0
  var longestEnd = 0
  string.indices.foreach { middle =>
    //ODD
    var left = middle
    var right = middle
    while (left >= 0 && right < string.length) {
      if (string(left) == string(right)) {
        if (right - left > longestEnd - longestStart) {
          longestStart = left
          longestEnd = right
        }
      }
      left -= 1
      right += 1
    }
    //EVEN
    left = middle
    right = middle + 1
    while (left >= 0 && right < string.length) {
      if (string(left) == string(right)){
        if (right - left > longestEnd - longestStart) {
          longestStart = left
          longestEnd = right
        }
      }
      left -= 1
      right += 1
    }
  }
  string.substring(longestStart, longestEnd + 1)
}
findLongestPalindrome("4123215")
findLongestPalindrome2("4123215")
