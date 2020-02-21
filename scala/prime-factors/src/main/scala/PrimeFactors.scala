import scala.collection.immutable.NumericRange
import scala.annotation.tailrec

object PrimeFactors {
    def factors(nr :Long)  : List[Long] = {
        def streamBy2(i: Long = 1): Stream[Long] = i #:: streamBy2(i + 2)
        val stream : Stream[Long] = 2 #:: streamBy2(3)

        @tailrec 
        def factorsRec(nr:Long, agg:List[Long]=Nil) :List[Long] = {
           nr match {
               case 1L => agg
               case x => 
                stream.takeWhile(_ <= math.sqrt(x)).filter(nr % _ == 0) match {
                    case Stream.Empty => nr::agg
                    case div#::_ => factorsRec(nr / div, div::agg)
                }
           }     
        }
        factorsRec(nr).sorted
    }

}