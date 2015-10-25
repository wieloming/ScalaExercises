package Algorithms

class SearchValueIn2DSortedArray {

  type Position = (Int, Int)
  type Array2D = Array[Array[Int]]

  def count(array: Array2D, key: Int): Option[Position] = {

    def count(array: Array2D, x: Int, y: Int, key: Int): Option[Position] = {
      if (isIn2DArray(array, (x, y))) {
        if (array(x)(y) > key) count(array, x - 1, y, key)
        else if (array(x)(y) < key) count(array, x, y + 1, key)
        else Some((x, y))
      } else {
        None
      }
    }

    def isIn2DArray(array: Array2D, position: Position): Boolean = position match {
      case (x, y) => x >= 0 && y <= array.head.length - 1
    }

    count(array, array.length - 1, 0, key)
  }


}
