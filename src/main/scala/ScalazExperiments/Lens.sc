import scalaz._

//Scala version:
case class Point(x: Int, y: Int)
case class Color(r: Int, g: Int, b: Int)
case class Turtle(position: Point, heading: Int, color: Color) {
  def forward(dist: Int): Turtle =
    copy(position =
      position.copy(
        x = position.x + dist * math.cos(heading).toInt,
        y = position.y + dist * math.sin(heading).toInt
      ))
}
val żółwik = Turtle(Point(2, 3), 0, Color(255, 255, 255))

//Scalaz magic:
val turtlePosition = Lens.lensu[Turtle, Point](
  (a, value) => a.copy(position = value),
  _.position
)
val pointX = Lens.lensu[Point, Int](
  (a, value) => a.copy(x = value),
  _.x
)
val turtleX = turtlePosition >=> pointX

turtleX.get(żółwik)
turtleX.set(żółwik, 15)