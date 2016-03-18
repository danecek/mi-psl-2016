class Rac(n: Int, d: Int) extends Ordered[Rac] {

  require(d != 0)

  override def compare(that: Rac): Int = this.nom * that.den - that.nom * this.den

  import Rac._

  val nom = n / nsd(n, d)
  val den = d / nsd(n, d)

  def this(n: Int) {
    this(n, 1)
  }

  def *(that: Rac) = new Rac(this.nom * that.nom, this.den * that.den)

  override def equals(that: Any): Boolean = that match {
    case x: Int => this.nom == x && this.den == 1
    case x: Rac => this.nom == x.nom && this.den == x.den
    case _ => false
  }

  def ===(that: Rac) = this.equals(that: Rac)

  override def toString() = s"Rac($nom,$den)"
}

object Rac {

  private def nsd(x: Int, y: Int): Int = {
    if (x < y) nsd(y, x)
    else if (y == 0) x
    else nsd(x % y, y)
  }

  def apply(n: Int, d: Int) = new Rac(n, d)

  implicit def int2Rac(x: Int) = Rac(x, 1)

}

