object Main {

  def mergeSort(x: Seq[Int]): Seq[Int] = {

    def merge(left: Seq[Int], right: Seq[Int]): Seq[Int] = {
      if (left.isEmpty) right
      else if (right.isEmpty) left
      else if (left.head > right.head)
        left.head +: merge(left.tail, right)
      else
        right.head +: merge(left, right.tail)
    }

    if (x.size == 1) x
    else {
      val (left, right) = x.splitAt(x.size / 2)
      merge(mergeSort(left), mergeSort(right))
    }

  }

  def selSort(x: Seq[Int]): Seq[Int] = {
    if (x.size == 1) x
    else {
      val min = x.min
      val fx = x filter (_ != min)
      selSort(fx).+:(min) // m +: selSort(xx);
    }

  }

  def main(args: Array[String]) {

    println(mergeSort(Seq(1, 5, 2, 23, 45, 1,3,5,100,100, 3,4,5)))
  }
}
