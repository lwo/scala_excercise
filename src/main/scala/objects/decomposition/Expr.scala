package week4.decomposition

trait Expr {
  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Product(n1, n2: Number) => eval(n1) * eval(n2)
    case Sum(n1, n2) => eval(n1) + eval(n2)
  }

  def show: String = {
    def parens(e: Expr) = e match {
      case Sum(_, _) => "(" + e.show + ")"
      case _ => e.show
    }
    this match {
      case Number(n) => n.toString
      case Sum(l, r) => l.show + " + " + r.show
      case Product(l, r) => parens(l) + " * " + parens(r)
      case Var(x) => x
    }
  }
}

case class Number(n: Int) extends Expr {}
case class Product(n1: Expr, n2: Expr) extends Expr {}
case class Sum(n1: Expr, n2: Expr) extends Expr {}
case class Var(s: String) extends Expr {}




object main {
  def main(args: Array[String]): Unit = {
    val o = new Expr {}
    println(o.eval(Product(Number(2), Number(2))))
    println(o.eval(Number(2))+o.eval(Number(5)))
    println("\nExpect: 2 * x + y")
    println("Actual: " + Sum(Product(Number(2), Var("x")), Var("y")).show)
    println("\nExpect: (2 + x) * y")
    println("Actual: " + Product(Sum(Number(2), Var("x")), Var("y")).show)
    println("Actual: " + Product(Product(Number(2), Var("x")), Var("y")).show)
  }
}