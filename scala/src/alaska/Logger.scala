/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package alaska

trait Logger extends ExprVisitor {
  abstract override def process(expr: Expr): String = {
    expr match {
      case Add(lhs, rhs) => println("Addition")
      case Sub(lhs, rhs) => println("Soustraction")
      case Const(n) => println("Constante : " + n.toString)
    }
    super.process(expr)
  }
}
