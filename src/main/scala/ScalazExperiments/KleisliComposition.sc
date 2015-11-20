import scalaz._
import Scalaz._

// Int => Option[String]
val f = Kleisli.kleisli[Option, Int, String]{
  (n: Int) => if (n % 2 == 0) None else Some((n + 1).toString)
}
// String => Option[String]
val g = Kleisli.kleisli[Option, String, String]{
  (s: String) => if (List(5, 7) any (_ == s.length)) None else Some("[" + s + "]")
}
// Kleisli composition
// Int => Option[String] => Option[String]
List(7, 78, 98, 99, 100, 102, 998, 999, 10000) map (f andThen g)
