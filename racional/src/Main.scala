/**
  * Created by danecek on 3/18/16.
  */
object Main {

    def main(args: Array[String]) {
      import Rac._
      println(Rac(24, 6))
      val r = Rac(2, 3)
      println(r == Rac(3,5))
      println(r == 2)
      println(2 === Rac(2, 1))
      println(2 > Rac(2, 1))
      println(2 >= Rac(2, 1))
    }



}
