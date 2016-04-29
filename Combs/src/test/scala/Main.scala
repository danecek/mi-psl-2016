
import scala.util.parsing.combinator._

object Arith extends JavaTokenParsers {
  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber | "("~expr~")"

  def wnv : Parser[Int] = wholeNumber ^^ ((x : String) => x.toInt) | failure("not a number")

  def sum : Parser[Int] = wnv~wnv ^^  {case x~y => x + y } // ((x : ~[Int, Int]) => x._1 + x._2)


  def main(args: Array[String]) {
    println(parseAll(wnv, "x"))
    println(parseAll(sum, "1 + 3 - 2"))
  }
}
