//stuff
class WheatField
class Mill(wheatField: WheatField)
class CowPasture
class DiaryFarm(cowPasture: CowPasture)
class Bakery(mill: Mill, dairyFarm: DiaryFarm){
  def bakeCake() = "cake!"
}

trait CropModule {
  lazy val wheatField = new WheatField()
  lazy val mill = new Mill(wheatField)
}
trait LivestockModule {
  lazy val cowPasture = new CowPasture()
  lazy val diaryFarm = new DiaryFarm(cowPasture)
}

object BakeMeCake extends CropModule with LivestockModule {
  lazy val bakery = new Bakery(mill, diaryFarm)
  val cake = bakery.bakeCake()
}


////////////////////////////////////////////////////
////////////////////////////////////////////////////
////////////////////////////////////////////////////

class EspressoMachine()
class Cafe(b: Bakery, e: EspressoMachine){
  def orderCoffeeAndCroissant() = "coffee!"
}
// composition: bakery depends on crop and livestock modules
trait BakeryModule extends CropModule with LivestockModule {
  lazy val bakery = new Bakery(mill, diaryFarm)
}
// abstract member: we need a bakery
trait CafeModule {
  lazy val espressoMachine = new EspressoMachine()
  lazy val cafe = new Cafe(bakery, espressoMachine)

  def bakery: Bakery
}
// the abstract bakery member is implemented in another module
object CafeApp extends CafeModule with BakeryModule {
  cafe.orderCoffeeAndCroissant()
}