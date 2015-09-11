def WHILE(condition: () => Boolean, command: () => Unit): Unit = {
  if (!condition()) return
  command()
  WHILE(condition, command)
}

var x = 5

WHILE(
  () => x > 0,
  () => {
    println(x)
    x -= 1
  }
)