object Chap16 {

  def isort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) Nil
    else insert(xs.head, isort(xs.tail))

  def insert(x: Int, xs: List[Int]): List[Int] =
    if (xs.isEmpty || x <= xs.head) x :: xs
    else xs.head :: insert(x, xs.tail)

  def insertPM(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys =>
      if (x <= y) x :: xs
      else y :: insert(x, ys)
  }

  def isortPM(xs: List[Int]): List[Int] = xs match {
    case List()  => List()
    case y :: ys => insertPM(y, isortPM(ys))
  }

  def append[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List()   => ys
    case x :: xs1 => x :: append(xs1, ys)
  }

  def rev[T](xs: List[T]): List[T] = xs match {
    case List()   => List()
    case x :: xs1 => rev(xs1) ::: List(x)
  }

  def msort[T](less: (T, T) => Boolean)(xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (x :: xs1, y :: ys1) =>
        if (less(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs.splitAt(n)
      merge(msort(less)(ys), msort(less)(zs))
    }
  }

  def reverseLeft[T](xs: List[T]): List[T] =
    (List[T]() /: xs){ (ys, y) => y :: ys}

  val num5 = List(4, 5, 2, 1, 3)
  val num2 = List(1, 2)
  isort(num5)
  isortPM(num5)
  append(num2, num5)
  rev(num5)
  num5.indices zip num5
  num5.zipWithIndex
  num5.mkString("[", ", ", "]")
  msort((x: Int, y: Int) => x < y)(num5)
  List.range(1, 5) flatMap (
    i => List.range(1, i) map (j => (i, j))
  )
  reverseLeft(num5)
  num5.sortWith(_ < _)
}
