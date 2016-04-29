import scala.collection.mutable.ListBuffer

abstract class Elem {
  val content: List[String]

  def height = content.length

  def width = content(0).size

  def widen(w: Int): Elem = {
    if (w <= width) this
    else this beside new CharElem(' ', w - width, height)
  }
  def heigen(h: Int): Elem = {
    if (h <= height) this
    else
    this above new CharElem(' ', width, h - height)
  }


  override def toString = content.mkString("\n")

  def above(e: Elem): Elem =
    if (width == e.width)
      new ArrayElem(content ++ e.content)
    else {
      val e1 = this.widen(e.width)
      val e2 = e.widen(width)
      e1 above e2
    }

  def beside(e: Elem): Elem ={
    if (height== e.height)

   new ArrayElem(for ((l, r) <- content zip e.content) yield l + r)
    else {
      val e1 = this.heigen(e.height)
      val e2 = e.heigen(height)

      e1 beside e2
    }
  }

}


class ArrayElem(override val content: List[String]) extends Elem


class CharElem(c: Char, w: Int, h: Int) extends Elem {

  val line = c.toString * w;
  val lb = new ListBuffer[String]
  for (i <- 0 until h) lb += line
  val content = lb.toList

}
