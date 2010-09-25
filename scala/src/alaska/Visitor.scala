package alaska

/** ExpressionVisitor interface */
abstract class ExprVisitor() {

  def process(expr: Expr): Unit

  protected val out: StringBuffer = new StringBuffer
  def result: String = out.toString
}

/**
 * ExpressionVisitor implementation displaying an expression using
 * a postfixed notation
 */
class PostfixedPrinter extends ExprVisitor {

  override def process(expr: Expr): Unit = expr match {
    case Add(lhs, rhs) => {
        process(lhs)
        out append " "
        process(rhs)
        out append " +"
      }
    case Sub(lhs, rhs) => {
        process(lhs)
        out append " "
        process(rhs)
        out append " -"
      }
    case Const(n) => out append n.toString
  }
}

/**
 * ExpressionVisitor implementation displaying an expression using
 * a prefixed notation
 */
class PrefixedPrinter extends ExprVisitor {

  override def process(expr: Expr): Unit = expr match {
    case Add(lhs, rhs) => {
        out append "+ "
        process(lhs)
        out append " "
        process(rhs)
      }
    case Sub(lhs, rhs) => {
        out append "- "
        process(lhs)
        out append " "
        process(rhs)
      }
    case Const(n) => out append n.toString
  }
}

class InfixedPrinter extends ExprVisitor {
  override def process(expr: Expr): Unit = expr match {
    case Add(lhs, rhs) => {
        out append "("
        process(lhs)
        out append " + "
        process(rhs)
        out append ")"
      }
    case Sub(lhs, rhs) => {
        out append "("
        process(lhs)
        out append " - "
        process(rhs)
        out append ")"
      }
    case Const(n) => out append n.toString
  }
}