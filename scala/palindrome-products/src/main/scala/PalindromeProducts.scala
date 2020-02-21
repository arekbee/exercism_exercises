import scala.annotation.tailrec

case class PalindromeProducts(x1:Int, x2:Int) {
    val range = x1 to x2
    val pair = for ( i <- range;j <- i to x2 if isPalindrom(i*j))  yield List(i,j) 
    val values = pair.map{ case List(i,j) =>  (i*j, (i, j))  }

    def isPalindrom(nr:Int) :Boolean = {
        @tailrec 
        def isPalindromRec(str:String) :Boolean  = {
            if(str.length > 1 ) { 
                val (left, middAndRight) = str.splitAt(1)
                val (midd, right) = middAndRight.splitAt(middAndRight.length-1)
                if(left == right) {
                    isPalindromRec(midd)
                }else false
            } else true
        }

        isPalindromRec(nr.toString)
    }

    type RType = Option[(Int, Set[(Int,Int)])]
    def smallest : RType  = {
        if(values.length > 0) {
            val selected = values.minBy(_._1)
            Some((selected._1, values.filter(_._1 == selected._1).map(_._2).toSet ))
        }
        else None

    }

    def largest : RType = {
        if(values.length > 0) {
            val selected = values.maxBy(_._1)
            Some((selected._1, values.filter(_._1 == selected._1).map(_._2).toSet ))
        }
        else None
    }
}