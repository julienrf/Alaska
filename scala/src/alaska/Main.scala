/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package alaska

object Main {

  def main(args: Array[String]): Unit = {
    var visitor = new PostfixPrinter
    ExprParser("3+2").accept(visitor)
  }

}
