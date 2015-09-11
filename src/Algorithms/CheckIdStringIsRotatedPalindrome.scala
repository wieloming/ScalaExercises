package Algorithms

class CheckIdStringIsRotatedPalindrome {

  def isRotatedPalindrome(string: String): Boolean = {

    def isRotatedPalindromeRec(chars: List[Char], countLeft: Int): Boolean = {
      if (countLeft <= 0) return false
      if (isPalindrome(chars)) return true
      isRotatedPalindromeRec(chars.tail :+ chars.head, countLeft - 1)
    }

    def isPalindrome(string: List[Char]): Boolean = {
      (string.size > 1) && string == string.reverse
    }

    isRotatedPalindromeRec(string.toList, string.length)
  }
}
