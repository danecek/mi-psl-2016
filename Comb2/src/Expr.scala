import scala.util.parsing.combinator._

object Expr extends JavaTokenParsers {
    def expr: Parser[Any] = term~rep("+"~term | "-"~term)
    def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
    def factor: Parser[Any] = floatingPointNumber | "("~expr~")"

  def mynumber : Parser[Int] = wholeNumber ^^ (_.toInt)

  def myterm : Parser[Any] = mynumber~mynumber ^^({
     case x ~ y => x + y
   })

    def main(args: Array[String]) {

      println(parseAll(expr, "2 + 3 - 4"))
    }


  }


