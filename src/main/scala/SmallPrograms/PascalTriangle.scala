package SmallPrograms

object PascalTriangle extends App {

  def goodPrintTriangle(height: Int) = {
    def printTree(height: Int) =
      (1 to height).foreach { rowNum =>
        print(" " * (height - rowNum))
        triangleRow(rowNum).foreach(num => print(num + " "))
        println()
      }

    def triangleRow(row: Int): List[Int] = {
      row match {
        case 1 => List(1)
        case 2 => List(1, 1)
        case n: Int =>
          List(1) ::: triangleRow(n - 1).sliding(2).map(_.sum).toList ::: List(1)
      }
    }
    printTree(height)
  }

  goodPrintTriangle(8)
}
