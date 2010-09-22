/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package alaska

/** Base class for any expression */
abstract class Expr {
  def accept(visitor: ExprVisitor): Unit
}

/** Integer constant */
case class Const(value:  Int) extends Expr {
  override def accept(visitor: ExprVisitor): Unit = {
    visitor.visitConst(this)
  }
}

/** Binary addition */
case class Add(lhs: Expr, rhs: Expr) extends Expr {
  override def accept(visitor: ExprVisitor): Unit = {
    visitor.visitAdd(this)
  }
}

/** Binary substraction */
case class Sub(lhs: Expr, rhs: Expr) extends Expr {
  override def accept(visitor: ExprVisitor): Unit = {
    visitor.visitSub(this)
  }
}