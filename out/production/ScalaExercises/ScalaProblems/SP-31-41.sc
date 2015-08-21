

//P31 (**) Determine whether a given integer number is prime.
implicit class BetterInt(num: Int) {
  def isPrime: Boolean = {
    for (n <- 2 until num) {
      if (num % n == 0)
        return false
    }
    true
  }

  def isCoprimeTo(num2: Int): Boolean = {
    gcd(num, num2) == 1
  }

  def totient: Int = {
    (0 until num).count(_.isCoprimeTo(num))
  }

  def primeFactors: List[Int] = {
    if (isPrime) {
      List(num)
    } else {
      (2 to num).toList.filter(_.isPrime).find(num % _ == 0) match {
        case Some(factor) => factor :: (num / factor).primeFactors
        case _ => throw new Exception("Could not find prime factor for " + num)
      }
    }
  }

  def primeFactorsMultiplicity: List[(Int, Int)] = {
    primeFactors.groupBy(_.toString).toList.map({ case (number, list) => (list.size, number.toInt) })
  }

  def goldbach: (Int, Int) = {
    (2 to num).filter(_.isPrime).map(pf => (pf, num - pf))
      .find({ case (a, b) => a.isPrime && b.isPrime }).get
  }
}

7.isPrime
//P32 (**) Determine the greatest common divisor of two positive integer numbers.
def gcd(num1: Int, num2: Int): Int = {
  num2 match {
    case 0 => num1
    case _ => gcd(num2, num1 % num2);
  }
}
gcd(36, 63)
//P33 (*) Determine whether two positive integer numbers are coprime.
//  Two numbers are coprime if their greatest common divisor equals 1.
35.isCoprimeTo(64)
//P34 (**) Calculate Euler's totient function phi(m).
10.totient
//P35 (**) Determine the prime factors of a given positive integer.
315.primeFactors
//P36 (**) Determine the prime factors of a given positive integer (2).
315.primeFactorsMultiplicity
//P39 (*) A list of prime numbers.
//Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
def listPrimesinRange(range: Range): List[Int] = {
  range.filter(_.isPrime).toList
}
listPrimesinRange(7 to 31)
//P40 (**) Goldbach's conjecture.
//Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than Scala's Int can represent). Write a function to find the two prime numbers that sum up to a given even integer.
28.goldbach
//P41 (**) A list of Goldbach compositions.
//Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
def printGoldbachList(range: Range) = {
  range.toList.filter(_ % 2 == 0).map(n => (n, n.goldbach)).foreach({
    case (number, gold) => println(number + " = " + gold._1 + " + " + gold._2)
  })
}
printGoldbachList(9 to 20)
//In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than, say, 50. Try to find out how many such cases there are in the range 2..3000.
def printGoldbachListLimited(range: Range, limit: Int) = {
  range.toList.filter(_ % 2 == 0).map(n => (n, n.goldbach))
    .filter({case(a, (b,c)) => b > limit && c > limit})
    .foreach({
    case (number, gold) => println(number + " = " + gold._1 + " + " + gold._2)
  })
}
printGoldbachListLimited(1 to 2000, 50)