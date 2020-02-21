object Grains {
    def square : PartialFunction[Int, Option[BigInt]] =  {
            case n:Int if 1 <= n  && n <= 64 => Some(BigInt(2).pow( n-1)) 
            case _ => None
        }
    def total = (1 to 64).flatMap(square(_)).sum
}