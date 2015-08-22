def getSequence(start: Int) = {
  val res = Stream.from(0).scanLeft(start)((sum, n: Int) => {
    if (sum % 2 == 0) sum / 2
    else 3 * sum + 1
  })
  println(res.force)
  res
}
//13 ? 40 ? 20 ? 10 ? 5 ? 16 ? 8 ? 4 ? 2 ? 1
(1 to 20).sortBy(getSequence(_).length).head
getSequence(13).take(10).length