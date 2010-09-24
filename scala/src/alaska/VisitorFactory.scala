/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package alaska

abstract class VisitorFactory {
  def createVisitor(): ExprVisitor
}

class PostfixedPrinterFactory extends VisitorFactory {
  override def createVisitor = new PostfixedPrinter
}

class PrefixedPrinterFactory extends VisitorFactory {
  override def createVisitor = new PrefixedPrinter
}