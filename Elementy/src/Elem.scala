abstract class Elem {
  def content: Seq[String]

  def width = content.head.length

  def height = content.size

  def above(e: Elem): Elem = new BasicElem(content ++ e.content)

  def beside(e: Elem): Elem = new BasicElem(
    // for (col <- content zip e.content) yield col._1 + col._2
    for ((f, s) <- content zip e.content) yield f + s
  )

  override def toString = content.mkString("\n")
}

class StringElemH(line : String) extends Elem {
  def content = Seq(line)
}

class StringElemV(line : String) extends Elem {
  def content = for (c <- line) yield c.toString
}

class BasicElem(val content: Seq[String]) extends Elem

class CharElem(c: Char, override val width: Int, override val height: Int) extends Elem {

  def content: Seq[String] = {
    val line: String = c.toString * width
    val cc = for (ui <- 1 to height) yield line
    cc
  }
}

object Main {

  def rec(n : Int) : Elem = {

    if (n == 1) new CharElem('*', 1, 1)
    else {
      if (n % 2 == 0) rec(n - 1) above(new CharElem('-', n, 1))
      else rec(n - 1) beside (new CharElem('|', 1, n))

    }

  }
  def main(args: Array[String]): Unit = {
    // println(new CharElem('-', 10, 1))
  //println(new CharElem('|', 1, 10))
   println(rec(10))

  }

}
