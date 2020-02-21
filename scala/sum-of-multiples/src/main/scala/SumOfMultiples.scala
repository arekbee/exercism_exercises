object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = {
    
    def getUniqueMultiples(fac :List[(Int, Int)] , multi:Int, multiples :Set[Int]) :Set[Int] = {
      fac.filter(_._2 < limit) match {
        case Nil => multiples
        case rest => {
            val newFac = rest.map(x=> (x._1, x._1 * (multi + 1)) )
            val newMultiples = multiples ++ rest.map(x=>x._2)
            getUniqueMultiples(newFac, multi + 1, newMultiples)
        }
      }
    }
    val facList = factors.toList.map(x=>(x,x))
    getUniqueMultiples(facList,1, Set(0) ).reduceLeft(_+_)
  }
}
