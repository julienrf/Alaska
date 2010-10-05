package alaska

/** Base class for any expression */
sealed abstract class Expr

/** Integer constant */
case class Const(value:  Int) extends Expr

/** Binary addition */
case class Add(lhs: Expr, rhs: Expr) extends Expr

/** Binary substraction */
case class Sub(lhs: Expr, rhs: Expr) extends Expr

/** Binary multiplication */
case class Mul(lhs: Expr, rhs: Expr) extends Expr

/** Binary division */
case class Div(lhs: Expr, rhs: Expr) extends Expr