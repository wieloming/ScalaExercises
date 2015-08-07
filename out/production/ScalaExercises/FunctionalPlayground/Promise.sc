

//class MyPromiseImpl<T> implements MyPromise<T> {
//  callback:(data:T)=>void;
//  then(callback) {
//    this.callback = callback;
//  }
//}
class PROMISE[T] {
  var callback = (result: T) => {}

  def THEN(callback: (T) => Unit): Unit = {
    this.callback = callback
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
  println("get test working")
  promise
}

getTest.THEN(
  (result) => {
    println("Dzia�a: " + result)
  }
)