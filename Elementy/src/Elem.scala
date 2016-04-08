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
    val cc = for (ui <- 1 to width) yield line
    cc
  }
}

object Main {
  def main(args: Array[String]): Unit = {

    println(new StringElemH("abcd"))
    println(new StringElemV("abcd"))

  }

}
