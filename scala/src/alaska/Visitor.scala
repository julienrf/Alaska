/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package alaska

/** ExpressionVisitor interface */
abstract class ExprVisitor {

  /* Je n’ai pas le droit de faire new ExprElmt(expr) car ExprElmt est abstraite */
  //implicit def ExprElmt(expr: Expr) = new ExprElmt(expr)
  /*implicit def ConstElmt(const: Const) = new ConstElmt(const)
  implicit def AddElmt(add: Add) = new AddElmt(add)
  implicit def SubElmt(sub: Sub) = new SubElmt(sub)*/

  def visitConst(const: Const)
  def visitAdd(add: Add)
  def visitSub(sub: Sub)
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
 * a postfix notation
 */
class PostfixPrinter extends ExprVisitor {

  override def visitConst(const: Const) {
    print(const.value.toString)
  }

  override def visitAdd(add: Add) {
    add.lhs.accept(this)
    print(" ")
    add.rhs.accept(this)
    print(" +")
  }

  override def visitSub(sub: Sub) {
    sub.lhs.accept(this)
    print(" ")
    sub.rhs.accept(this)
    print(" -")
  }
}
