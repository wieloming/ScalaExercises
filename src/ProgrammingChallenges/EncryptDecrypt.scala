package ProgrammingChallenges

object EncryptDecrypt extends App {
  def askForEncryptDecrypt(): String = {
    val input = io.StdIn.readLine()
    if (!List("E", "D").contains(input)) {
      println("enter E or D")
      askForEncryptDecrypt()
    } else {
      input
    }
  }

  val lettersConnection = "0"
  val spaceReplacement = 33
  val letterMultiplier = 13
  val sequenceMultiplier = 2

  def encrypt(string: String): String = {
    def letterToNumber(char: Char, orElse: Int): Int = {
      val dict = ('A' to 'Z').zipWithIndex.toMap
      dict.getOrElse(char.toUpper, orElse)
    }

    BigInt(
      string
        .map(letterToNumber(_, spaceReplacement))
        .map(_ * letterMultiplier)
        .mkString(lettersConnection)
        * sequenceMultiplier
    ).toString()
  }

  def decrypt(string: String): String = {
    def numberToLetter(int: Int, orElse: Char): Char = {
      val dict = ('A' to 'Z').zipWithIndex.toMap.map(_.swap)
      dict.getOrElse(int, orElse)
    }

    (BigInt(string) / sequenceMultiplier)
      .toString()
      .split(lettersConnection)
      .filterNot(_ == "")
      .map(_.toInt / letterMultiplier)
      .map(numberToLetter(_, ' '))
      .mkString("")

  }

  println("enter message:")
  val input = io.StdIn.readLine()
  println("do you want to encrypt[E] or decrypt[D]")
  val operationType = askForEncryptDecrypt()
  operationType match {
    case "E" => println(encrypt(input))
    case "D" => println(decrypt(input))
  }

}
