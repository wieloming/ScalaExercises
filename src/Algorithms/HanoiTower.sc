def move(disks: Int, FROM: Int, TO: Int, VIA: Int) : Unit = {
  if (disks == 1) {
    println("From " + FROM + " to " + TO)
  } else {
    move(disks - 1, FROM, VIA, TO) // move previous from FROM to VIA by TO
    move(1, FROM, TO, VIA) // move one to TO by VIA
    move(disks - 1, VIA, TO, FROM) // move previous from VIA to TO by FROM
  }
}

move(2, 1, 2, 3)