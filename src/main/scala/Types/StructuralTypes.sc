type OpenerCloser = {
  def open(): Unit
  def close(): Unit
}

def on(it: OpenerCloser, fun: OpenerCloser => Unit) = {
  it.open()
  fun(it)
  it.close()
}