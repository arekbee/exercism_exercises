object NumberType extends Enumeration {
    sealed trait NumberTypeEnum
    case object Perfect extends NumberTypeEnum
    case object Abundant extends NumberTypeEnum
    case object Deficient extends NumberTypeEnum
}

object PerfectNumbers {
    def classify(n :Int) :  Either[String, NumberType.NumberTypeEnum] = {
            n match {
                case n if n>0 => {
                    getFactorsSum(n) match {
                        case fsum if fsum == n => Right(NumberType.Perfect)
                        case fsum if fsum > n =>  Right(NumberType.Abundant)
                        case _ => Right(NumberType.Deficient)
                    }
                }
                case _ => Left("Classification is only possible for natural numbers.")
            }

    }
    def getFactorsSum(n :Int) : Int = {
        def getFactorsRec(i:Int, fac:Set[Int]) : Set[Int] = {
            i match {
                case i if i > n/2 => fac
                case i if n % i == 0 =>  getFactorsRec(i+1, fac + i + (n/i) )
                case _ => getFactorsRec(i+1, fac )
            }
        }
        n match {
            case 1 => 0
            case _ => getFactorsRec(2, Set(1)).sum
        }
    }
    
}