import scala.annotation.tailrec

object AllYourBase {
    implicit class PowerInt(val i:Double) extends AnyVal {
        def ** (exp:Double):Double = math.pow(i,exp)
    }

    def rebase(a:Int, digits:List[Int], b:Int) : Option[List[Int]] = {
        if(a <2 || b < 2) None
        else if (digits.exists(x=> x>=a || x < 0)) None
        else {
            val sum = digits.reverse.zipWithIndex.collect{case (d,i)=> d * (a**i)}.sum.toInt
            Some(getDigits(sum, b))
        }
    }
    def getDigits(n:Int, base:Int) : List[Int] = {
        @tailrec
        def getDigitsRec(n:Int, agg:List[Int]=Nil) :List[Int] = {
            n match {
                case 0  =>  agg
                case n => getDigitsRec(n/base, (n % base) :: agg)
            }    
        }
        n match {
            case 0 => List(0)
            case n => getDigitsRec(n)
        }
    }
}