//FROM: http://blog.rafaelferreira.net/2008/07/type-safe-builder-pattern-in-scala.html
object TypeSafeBuilderPattern {
  sealed abstract class Preparation

  case object Neat extends Preparation
  case object OnTheRocks extends Preparation
  case object WithWater extends Preparation

  sealed abstract class Glass
  case object Short extends Glass
  case object Tall extends Glass
  case object Tulip extends Glass

  case class OrderOfScotch(brand: String, mode: Preparation, isDouble: Boolean, glass: Option[Glass])

  abstract class TRUE
  abstract class FALSE

  class ScotchBuilder[HB, HM, HD](val theBrand: Option[String],
                                  val theMode: Option[Preparation],
                                  val theDoubleStatus: Option[Boolean],
                                  val theGlass: Option[Glass]) {
    def withBrand(b: String) =
      new ScotchBuilder[TRUE, HM, HD](Some(b), theMode, theDoubleStatus, theGlass)
    def withMode(p: Preparation) =
      new ScotchBuilder[HB, TRUE, HD](theBrand, Some(p), theDoubleStatus, theGlass)
    def isDouble(b: Boolean) =
      new ScotchBuilder[HB, HM, TRUE](theBrand, theMode, Some(b), theGlass)
    def withGlass(g: Glass) = new ScotchBuilder[HB, HM, HD](theBrand, theMode, theDoubleStatus, Some(g))
  }

  implicit def enableBuild(builder: ScotchBuilder[TRUE, TRUE, TRUE]): Object {def build(): OrderOfScotch} = new {
    def build() =
      new OrderOfScotch(builder.theBrand.get, builder.theMode.get, builder.theDoubleStatus.get, builder.theGlass);
  }

  def builder = new ScotchBuilder[FALSE, FALSE, FALSE](None, None, None, None)

  builder.withBrand("pac").withMode(OnTheRocks).isDouble(true).build()
}