package alaska

/**
 * Core class.
 * Alaska can process a given expression using a given strategy.
 */
class Alaska(private var _visitor: ExprVisitor) {

  def process(expr: Expr): String = visitor process expr

  def visitor = _visitor
  def visitor_=(v: ExprVisitor): Unit = _visitor = v
}
