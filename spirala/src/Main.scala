/**
  * Created by danecek on 4/1/16.
  */
object Main {

  def spirala(n: Int): Elem = {
    if (n == 1) new CharElem('*', 1, 1)
    else {
      val s = spirala(n - 1)
      println(n-1 + "--------------")
      println(s)
      n % 4 match {
        case 0 => s above new CharElem('*', n, 1)
        case 1 => s beside new CharElem('*', 1, n)
        case 2 => new CharElem('*', n, 1) above s
        case 3 => s beside new CharElem('*', 1, n)
        case _ => error("xxxx")
      }
    }
  }

  def main(args: Array[String]) {
    spirala(6)
    //   println(new CharElem('x', 10, 20))
  }
}
