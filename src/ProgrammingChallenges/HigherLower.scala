package ProgrammingChallenges

import scala.util.Random

object HigherLower extends App {

  implicit class printable(string: String){
      def -> = println(string)
  }

  def getInput: String = {
    val input = io.StdIn.readLine()
    if (input.isEmpty || !(input forall Character.isDigit) || input.toInt <= 1) {
      "please enter a digit bigger than 1...".->
      getInput
    } else {
      input
    }
  }

  def game(randomNumber: Int, numbers: List[Int]): Unit = {
    if(numbers.length > 1) {
      ("current number is " + randomNumber).->
      (numbers + " numbers in the box, next number will be higher(H) or lower(L)?").->
      val nextNumber = numbers(Random.nextInt(numbers.size - 1))
      askForHigherLower(randomNumber, nextNumber, numbers)
      game(nextNumber, numbers.filterNot(_ == nextNumber))
    }else{
      "thanks for playing :)".->
    }
  }

  def askForHigherLower(randomNumber: Int, nextNumber: Int, numbers: List[Int]): Unit = {
    val input = io.StdIn.readLine()
    if (!List("L", "H").contains(input)) {
      "enter L or H".->
      askForHigherLower(randomNumber, nextNumber, numbers)
    } else {
      if (nextNumber > randomNumber && input == "H" || nextNumber < randomNumber && input == "L") "bravo!".->
      else "maybe next time...".->
    }
  }

  "type the upper bound...".->
  val upperBound = getInput.toInt
  val firstRandomNumber = Random.nextInt(upperBound)
  val numbers = (0 to upperBound).filterNot(_ == firstRandomNumber).toList

  game(firstRandomNumber, numbers)


}
