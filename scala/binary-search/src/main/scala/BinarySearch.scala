object BinarySearch {
    def find(l:List[Int], search:Int, baseIdx:Int = 0) : Option[Int] = {
        l  match {
            case Nil => None
            case List(inL) if inL != search => None
            case lm => {
                val midIdx = l.size / 2
                l(midIdx) match {
                    case `search` => Some(baseIdx + midIdx)
                    case x => {
                        val split = l.splitAt(midIdx)
                        if (x > search)  find(split._1, search, baseIdx)
                        else find(split._2, search, baseIdx+midIdx)
                    }  
                }
            }
        }
    }
}