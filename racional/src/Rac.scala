
trait C[T <: C[T]] extends Comparable[T] {
  this: T =>
  def <(that: T): Boolean = this.compareTo(that) < 0

  def >=(that: T) = !(that < this)

}

class Rac(n: Int, d: Int) extends C[Rac] {

  def compareTo(that: Rac) = 1

  def nsd = 1

  val nom = n / nsd
  val den = d / nsd

  def *(r: Rac) = Rac(this.nom * r.nom, this.den * r.den)

  override def equals(that: Any) = eq(that.asInstanceOf[AnyRef])


}


object Rac {


  implicit class AsRac(v : Int) {
    def === (r : Rac, x : Int = 1) = {
      println("my==")
      true

    }

  }

  def apply(n: Int, d: Int) = {
    println(n.asInstanceOf[Double] / d)
    new Rac(n, d)
  }


  def main(args: Array[String]) {
    import Rac._
    val r = Rac(2, 3)
    println(r == r)
    println(2 .=== (r, 1))
  }

}


