class Accumulate {
  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] = {
    list match {
      case x :: l => f(x) :: accumulate(f,l)
      case Nil => Nil
    }
  }
}
