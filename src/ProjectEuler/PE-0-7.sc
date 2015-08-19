
//Find the sum of all the multiples of 3 or 5 below 1000
def sumOfAllMultiples(limit: Int, numbers: Int*) = {
  numbers.flatMap(n => (1 until limit).toList.filter(_ % n == 0)).distinct.sum
}
sumOfAllMultiples(1000, 3, 5)

//Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
val fibs: Stream[Int] = 0 #:: fibs.scanLeft(1)(_ + _)
val fibs2: Stream[Int] = 0 #:: 1 #:: fibs2.zip(fibs2.tail).map(t => t._1 + t._2)
fibs
  .filter(_ % 2 == 0)
  .takeWhile(_ < 4000000).sum

//What is the largest prime factor of the number 600851475143 ?
//TODO: problem with to big long number
//def tmp(number: Long) = {
//  (2L to number)
//    .filter(_ % number == 0L)
//    .sum
//}

//Find the largest palindrome made from the product of two 3-digit numbers.
def isPalindrome(num: Int): Boolean = {
  num.toString == num.toString.reverse
}
(for {
  x <- 1 to 999
  y <- 1 to 999
  if isPalindrome(x * y)
} yield x * y).max
//What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
def isDivisibleByAllTo20(num: Int) = {
  (1 to 20).forall(num % _ == 0)
}
//TODO: make it faaaaster (need maths)
//Stream.from(1, 20).find(isDivisibleByAllTo20).get

//Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
def sumOfSquaresVsSquareOfSums(limit: Int) = {
  (1 to limit).sum * (1 to limit).sum - (1 to limit).map(x => x * x).sum
}
sumOfSquaresVsSquareOfSums(100)

//What is the 10 001st prime number?
def isPrime(n: Int) = (2 until n - 1) forall (n % _ != 0)
Iterator.from(2).filter(isPrime).drop(10000).next()
