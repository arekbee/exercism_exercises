object RomanNumerals {
    val numbers = Map(1->"I", 5->"V", 10-> "X", 50-> "L", 100-> "C", 500-> "D", 1000->"M")

    private def getTenths(nr:Int) = math.floor(math.log10(nr)).toInt

    def roman(nr:Int) :String = {
        unfoldRight(nr.toString) { nrStr =>  
            val nr = nrStr.toInt
            if (nr ==0 ) {
                None
            }  
            else {
                val tenth = math.pow(10, getTenths(nr)).toInt
                val mul = math.floor(nr / tenth).toInt
                val rest = nr  - tenth * mul
                val baseSymbol =  numbers(tenth)
                
                val symbol = mul match  {
                    case mul if mul < 4 => baseSymbol * mul
                    case 4 => baseSymbol + numbers(tenth*5)
                    case 5 => numbers(tenth*5)
                    case mul if mul < 9 => numbers(tenth*5)  + baseSymbol * (mul-5)
                    case 9 => baseSymbol + numbers(tenth*10)
                }
                Some( symbol, rest.toString)
             }
        }.mkString
    }

    private def unfoldRight[A, B](seed: B)(f: B => Option[(A, B)]): List[A] = f(seed) match {
        case Some((a, b)) => a :: unfoldRight(b)(f)
        case None => Nil
    }
        
    private def unfoldLeft[A, B](seed: B)(f: B => Option[(B, A)]) = {
        def loop(seed: B)(ls: List[A]): List[A] = f(seed) match {
         case Some((b, a)) => loop(b)(a :: ls)
         case None => ls
        }
        loop(seed)(Nil)
    }
}