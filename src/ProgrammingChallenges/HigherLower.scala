package ProgrammingChallenges

import scala.util.Random

object HigherLower extends App {

  implicit class Printable(string: String){
      def -> = println(string)
  }

  private def getIntInput: Int = {
    val input = io.StdIn.readLine()
    if (input.isEmpty || !(input forall Character.isDigit) || input.toInt <= 1) {
      "please enter a digit bigger than 1...".->
      getIntInput
    } else {
      input.toInt
    }
  }

  def game(randomNumber: Int, numbers: List[Int], points: Int): Unit = {
    if(numbers.length > 1) {
      ("current number is " + randomNumber).->
      (numbers + " numbers in the box, next number will be higher(H) or lower(L)?").->
      val nextNumber = numbers(Random.nextInt(numbers.size - 1))
      val newPoints = askForHigherLower(randomNumber, nextNumber, numbers, points)
      game(nextNumber, numbers.filterNot(_ == nextNumber), newPoints)
    }else{
      ("thanks for playing, you earned " + points + " p.").->
    }
  }

  def askForHigherLower(randomNumber: Int, nextNumber: Int, numbers: List[Int], points: Int): Int = {
    val input = io.StdIn.readLine()
    if (!List("L", "H").contains(input)) {
      "enter L or H".->
      askForHigherLower(randomNumber, nextNumber, numbers, points)
    } else {
      if (nextNumber > randomNumber && input == "H" || nextNumber < randomNumber && input == "L") {
        "bravo!".->
        return points + 1
      } else
        "maybe next time...".->
         points
    }
  }

  "type the upper bound...".->
  val upperBound = getIntInput
  val firstRandomNumber = Random.nextInt(upperBound)
  val numbersInGame = (0 to upperBound).filterNot(_ == firstRandomNumber).toList

  game(firstRandomNumber, numbersInGame, 0)


}
