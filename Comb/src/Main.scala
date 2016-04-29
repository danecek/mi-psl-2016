import scala.util.parsing.combinator._

object Arith extends JavaTokenParsers {
  // def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  //  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber // | "("~expr~")"



  def mwnc: Parser[Int] = mwn ~ mwn ^^ { case x ~ y => x + y } //((x : ~[Int, Int]) => x._1 + x._2)

  def f(l: List[~[String, Int]]) = {
    val pl: List[Int] = for (c <- l; if c._1 == "+") yield c._2
    val mn: List[Int] = for (c <- l; if c._1 == "-") yield c._2
    pl.sum - mn.sum
  }

  def f2(l: List[~[String, Int]]) : Int = {
    if (l.isEmpty) 0
    else {
      val (x ~ y) = l.head
      if (x == "+") y + f(l.tail)
      else if (x == "-") - y + f(l.tail)
      else throw new RuntimeException
    }

  }

  def f3(l: List[~[String, Int]]) : Int = {
    l match {
      case ("+"~y :: tail) =>  y + f2(tail)
      case ("-"~y :: tail) => - y + f2(tail)
      case Nil =>0
    }

  }

  def mwn: Parser[Int] = wholeNumber ^^ (_.toInt)
  def elem : Parser[Int]  = ("-"|"+")~mwn ^^ ({case "+" ~x => x
                                              case "-"~y => -y})
  def myterm = rep(elem) ^^ (_.sum)




  def main(args: Array[String]) {
    println(parseAll(myterm, "- 1 + 5 - 3 + 8"))
  }
}
