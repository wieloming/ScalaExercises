import shapeless._
import scala.util.Try

case class Person(name: String, age: Double)
case class Book(title: String, author: String, year: Int)

case class Country(name: String, population: Int, area: Double)
Generic[Person]

trait Parser[A] {
  def apply(s: String): Option[A]
}

implicit val stringParser: Parser[String] = new Parser[String] {
  def apply(s: String): Option[String] = Some(s)
}
implicit val intParser: Parser[Int] = new Parser[Int] {
  def apply(s: String): Option[Int] = Try(s.toInt).toOption
}
implicit val doubleParser: Parser[Double] = new Parser[Double] {
  def apply(s: String): Option[Double] = Try(s.toDouble).toOption
}
implicit val hnilParser: Parser[HNil] = new Parser[HNil] {
  def apply(s: String): Option[HNil] = if (s.isEmpty) Some(HNil) else None
}
implicit def hconsParser[H: Parser, T <: HList : Parser]: Parser[H :: T] = new Parser[H :: T] {
  def apply(s: String): Option[H :: T] = s.split(",").toList match {
    case cell +: rest => for {
      head <- implicitly[Parser[H]].apply(cell)
      tail <- implicitly[Parser[T]].apply(rest.mkString(","))
    } yield head :: tail
  }
}
// here happens some really strange magic
implicit def classParser[A, R <: HList](implicit gen: Generic[A] {type Repr = R},  parser: Parser[R]): Parser[A] = new Parser[A] {
  //.map(gen.from) changes from Option[R](returned by parser) to Option[A]
  def apply(s: String): Option[A] = parser(s).map(gen.from)
}
object Parser {
  def apply[A](s: String)(implicit parser: Parser[A]): Option[A] = parser(s)
}

Parser[Person]("Amy,54.2")
Parser[Book]("Hamlet,Shakespeare,1600")
Parser[Book]("Hamlet,Shakespeare")