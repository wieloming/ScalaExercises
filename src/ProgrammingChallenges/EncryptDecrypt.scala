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

  val key = 1 to 26
  val keyMap = ('a' to 'z' zip key).toMap
  val reverseKeyMap: Map[Int, Char] = keyMap map (_.swap)

  def encrypt(string: String): String = {
    string
      .split(" ")
      .map { word =>
      word
        .toLowerCase
        .replaceAll("[^\\p{L}\\p{Nd}]+", "")
        .map(keyMap)
        .zip(key)
        .map({ case (value, keyVal) => ((value + keyVal) % 26) + 1 })
        .map(reverseKeyMap)
        .mkString
    }
      .mkString(" ")
  }

  def decrypt(string: String): String = {
    string.split(" ").map { word =>
      word
        .map(keyMap)
        .zip(key)
        .map({ case (value, keyVal) =>
        val diff = value - keyVal - 1
        if (diff < 1) 26 + diff
        else diff
      })
        .map(reverseKeyMap)
        .mkString
    }
      .mkString(" ")
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
