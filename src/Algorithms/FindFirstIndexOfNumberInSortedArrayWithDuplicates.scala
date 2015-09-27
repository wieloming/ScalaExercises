package Algorithms

class FindFirstIndexOfNumberInSortedArrayWithDuplicates {
  def binarySearchFunctional(list: Array[Int], target: Int): Int = {
    def bsf(list: Array[Int], target: Int, start: Int, end: Int): Int = {
      if (start > end) return -1
      val mid = start + (end - start + 1) / 2
      if (list(mid) == target) {
        var firstMid = mid
        while (list(firstMid) == target) {
          firstMid -= 1
        }
        firstMid
      } else if (list(mid) > target) {
        bsf(list, target, start, mid - 1)
      } else {
        bsf(list, target, mid + 1, end)
      }
    }
    bsf(list, target, 0, list.length - 1)
  }
}
