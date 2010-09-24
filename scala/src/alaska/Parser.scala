package alaska

import scala.util.parsing.combinator.syntactical._

object ExprParser extends StandardTokenParsers {
  lexical.delimiters ++= List("+", "-", "*", "/", "(", ")")

  def expr = term*(
      "+" ^^^ {(lhs, rhs) => Add(lhs, rhs)}
    | "-" ^^^ {(lhs, rhs) => Sub(lhs, rhs)})
  def term = factor
  def factor: Parser[Expr] = const | "(" ~> expr <~ ")"
  def const: Parser[Expr] = numericLit ^^ { s => Const(s.toInt) }

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
