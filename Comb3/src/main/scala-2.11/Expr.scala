import scala.util.parsing.combinator._

object Expr extends JavaTokenParsers {
  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber | "("~expr~")"

  def main(args: Array[String]) {
    println(parseAll(expr, "1+2"))
  }


}

