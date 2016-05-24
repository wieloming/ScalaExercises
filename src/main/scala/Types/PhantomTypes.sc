trait Op
trait Open extends Op
trait Close extends Op

trait Door[O <: Op]
object Door {
  def apply[S <: Op] = new Door[S] {}

  def open[S <: Close](d: Door[S]) = Door[Open]
  def close[S <: Open](d: Door[S]) = Door[Close]
}

val closeDoor = Door[Close]
val openDoor = Door.open(closeDoor)
val closeAgainDoor = Door.close(openDoor)

val closeCloseDoor = Door.close(closeDoor)
val openOpenDoor = Door.open(openDoor)