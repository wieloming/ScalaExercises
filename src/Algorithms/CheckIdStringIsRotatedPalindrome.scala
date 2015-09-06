package Algorithms

class CheckIdStringIsRotatedPalindrome {

  def isRotatedPalindrome(string: String): Boolean = {
    isRotatedPalindromeRec(string.toList, string.length)
  }

  private def isRotatedPalindromeRec(chars: List[Char], countLeft: Int): Boolean = {
    if (countLeft <= 0) return false
    if (isPalindrome(chars)) return true
    isRotatedPalindromeRec(chars.tail :+ chars.head, countLeft - 1)
  }

  private def isPalindrome(string: List[Char]): Boolean = {
    (string.size > 1) && string == string.reverse
  }
}
