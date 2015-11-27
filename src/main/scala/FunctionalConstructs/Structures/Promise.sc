class PROMISE[T] {
  var callback = (result: T) => {}

  def THEN(newCallback: (T) => Unit): Unit = {
    this.callback = newCallback
  }
}

def getTest: PROMISE[String] = {
  val promise: PROMISE[String] = new PROMISE

  val thread = new Thread {
    override def run() {
      println("Thread starts and goes to sleep")
      Thread sleep 5000
      println("Thread wakes up")
      promise.callback("test")
    }
  }

  thread.start()
  thread.join(5000)
  println("get test working")
  promise
}

getTest.THEN(
  (result) => {
    println("Działa: " + result)
  }
)
