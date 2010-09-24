package alaska

/** ExpressionVisitor interface */
abstract class ExprVisitor() {

  /* Je n’ai pas le droit de faire new ExprElmt(expr) car ExprElmt est abstraite */
  //implicit def ExprElmt(expr: Expr) = new ExprElmt(expr)
  /*implicit def ConstElmt(const: Const) = new ConstElmt(const)
  implicit def AddElmt(add: Add) = new AddElmt(add)
  implicit def SubElmt(sub: Sub) = new SubElmt(sub)*/
  def visitConst(const: Const)
  def visitAdd(add: Add)
  def visitSub(sub: Sub)

  protected val out: StringBuffer = new StringBuffer
  def result: String = out.toString
}

/*abstract class ExprElmt(expr: Expr) {
  def accept(visitor: ExprVisitor): Unit
}

class ConstElmt(const: Const) extends ExprElmt(const) {
  override def accept(visitor: ExprVisitor): Unit = {
    visitor.visitConst(const)
  }
}

class AddElmt(add: Add) extends ExprElmt(add) {
  override def accept(visitor: ExprVisitor): Unit = {
    visitor.visitAdd(add)
  }
}

class SubElmt(sub: Sub) extends ExprElmt(sub) {
  override def accept(visitor: ExprVisitor): Unit = {
    visitor.visitSub(sub)
  }
}*/


/**
 * ExpressionVisitor implementation displaying an expression using
 * a postfixed notation
 */
class PostfixedPrinter extends ExprVisitor {

  override def visitConst(const: Const) {
    out append const.value.toString
  }

  override def visitAdd(add: Add) {
    add.lhs accept this
    out append " "
    add.rhs accept this
    out append " +"
  }

  override def visitSub(sub: Sub) {
    sub.lhs accept this
    out append " "
    sub.rhs accept this
    out append " -"
  }
}

/**
 * ExpressionVisitor implementation displaying an expression using
 * a prefixed notation
 */
class PrefixedPrinter extends ExprVisitor {

  override def visitConst(const: Const) {
    out append const.value.toString
  }

  override def visitAdd(add: Add) {
    out append "+ "
    add.lhs accept this
    out append " "
    add.rhs accept this
  }

  override def visitSub(sub: Sub) {
    out append "- "
    sub.lhs accept this
    out append " "
    sub.rhs accept this
  }
}

class InfixedPrinter {
  def process(expr: Expr): String = expr match {
    case Add(lhs, rhs) => "(" + process(lhs) +  " + " + process(rhs) + ")"
    case Sub(lhs, rhs) => "(" + process(lhs) +  " - " + process(rhs) + ")"
    case Const(n) => n.toString
  }
}