class Entity {
  def persistForReal() = println("Wszystko wybucha i ogniem zieje")
}

trait Persister {
  def doPersist(e: Entity) = e.persistForReal()
}

// our refined instance (and type):
val refinedMockPersister = new Persister {
  override def doPersist(e: Entity) = ()
}
val entity = new Entity

refinedMockPersister.doPersist(entity)
