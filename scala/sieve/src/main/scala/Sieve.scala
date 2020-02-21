object Sieve {
    def primes(toNum:Int) :List[Int] = {
        val arr = new Array[Boolean](toNum+1)
        for(i <- 2 to toNum ; inner <- 2 to toNum/i) arr(inner*i) = true
        arr.zipWithIndex.filter(!_._1).filter(_._2>1).map(_._2).toList
    }
}