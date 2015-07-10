package Algorithms

class ConvertDoubleToRational {

  def count(number: Double): Unit = {
    var base = 1
    while (number * base != (number * base).toInt) {
      base *= 10
    }
    val numberToDivide = (number * base).toInt
    val largestCommonDiv = largestCommonDivisor(numberToDivide, base)
    println(numberToDivide / largestCommonDiv + " / " + base / largestCommonDiv)
  }

  private def largestCommonDivisor(number: Int, number2: Int): Int = {
    (1 to Math.min(number, number2)).filter(x => number % x == 0 && number2 % x == 0).max
  }
}
