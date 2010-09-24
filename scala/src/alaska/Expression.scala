package alaska

/** Base class for any expression */
sealed abstract class Expr {
  
  def accept(visitor: ExprVisitor): Unit
}

/** Integer constant */
case class Const(value:  Int) extends Expr {

  override def accept(visitor: ExprVisitor): Unit = {
    visitor visitConst this
  }
}

/** Binary addition */
case class Add(lhs: Expr, rhs: Expr) extends Expr {

  override def accept(visitor: ExprVisitor): Unit = {
    visitor visitAdd this
  }
}

/** Binary substraction */
case class Sub(lhs: Expr, rhs: Expr) extends Expr {

  override def accept(visitor: ExprVisitor): Unit = {
    visitor visitSub this
  }
}