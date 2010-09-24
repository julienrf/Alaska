package alaska

/**
 * Core class.
 * Alaska can process a given expression using a given strategy.
 */
class Alaska(private var _visitorFactory: VisitorFactory) {

  def process(expr: Expr): String = {
    val visitor = visitorFactory.createVisitor
    expr accept visitor
    visitor.result
  }

  def visitorFactory = _visitorFactory
  def visitorFactory_=(visitorFactory: VisitorFactory): Unit = {
    _visitorFactory = visitorFactory
  }
}
