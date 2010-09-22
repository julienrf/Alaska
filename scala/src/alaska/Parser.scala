/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package alaska

import scala.util.parsing.combinator.syntactical._

object ExprParser extends StandardTokenParsers {

  lexical.delimiters ++= List("+", "-")

  def expr = sum | term
  def sum = add | sub
  def add: Parser[Expr] = term ~ "+" ~ term ^^ { case lhs ~ _ ~ rhs =>
      Add(lhs, rhs) }
  def sub: Parser[Expr] = term ~ "-" ~ term ^^ { case lhs ~ _ ~ rhs =>
      Sub(lhs, rhs) }
  def term: Parser[Expr] = const | "(" ~> expr <~ ")"
  def const = numericLit ^^ { s => Const(s.toInt) }

  def parse(s: String) = {
    val tokens = new lexical.Scanner(s)
    phrase(expr)(tokens)
  }

  def apply(s: String): Expr = {
    parse(s) match {
      case Success(tree, _) => tree
      case e: NoSuccess =>
        throw new IllegalArgumentException("Syntax error: " + s)
    }
  }

  def test(exprstr: String) = {
    parse(exprstr) match {
      case Success(tree, _) =>
        println("Ok")
      case e: NoSuccess => Console.err.println(e)
    }
  }
}
