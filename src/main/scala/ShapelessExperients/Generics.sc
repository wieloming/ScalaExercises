import shapeless._
import ops.hlist._
import syntax.std.function._
import ops.function._

implicit class Expression[P <: Product, G <: HList, F, L <: HList, R](tuple: P) {
  def eval(implicit gen: Generic.Aux[P, G], ihc: IsHCons.Aux[G, F, L], ftp: FnToProduct.Aux[F, L => R]) = {
    val list = gen.to(tuple)
    val (func, args) = (list.head, list.tail)
    func.toProduct(args)
  }
}

def add = (a: Int, b: Int) => a + b
def stringLength = (_: String).length

(add, 1, 2).eval // Int = 3
(stringLength, "foobar").eval // Int = 6
