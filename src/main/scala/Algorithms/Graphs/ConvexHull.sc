case class Point(x: Int, y: Int)


def grahamScan(points: List[Point]): List[Point] = {

  //is way a->b->c turning counterwise
  def isCounterWise(a: Point, b: Point, c: Point): Boolean = {
    (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x) > 0
  }

  def scan(points: List[Point]): List[Point] = points match {
    case Nil => List()
    case x :: y :: Nil => points
    case x :: y :: z :: xs if isCounterWise(x, y, z) => x :: scan(y :: z :: xs)
    case x :: y :: z :: xs => scan(x :: z :: xs)
  }
  //find the coordinate with the lowest latitude
  val origin = points.minBy(_.y)
  //sort the rest of the points according to their polar angle (the angle between the line
  //defined by the origin and the current point, and the x-axis)
  val pointsSorted = origin :: points.filterNot(_ == origin).
    sortBy(point => Math.atan2(point.y - origin.y, point.x - origin.x))
  //do the graham scan
  scan(pointsSorted)

}