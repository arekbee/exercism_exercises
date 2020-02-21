case class Triangle(b1 :Double,b2:Double,b3:Double) {
    def isosceles : Boolean =   (b1 == b2 || b1 == b3 || b2==b3 ) && canBuildTriangle(b1,b2,b3) && allPositive(b1,b2,b3)
    def equilateral: Boolean = (b1!=0 && b1 == b2 && b1 == b3 ) &&  canBuildTriangle(b1,b2,b3) && allPositive(b1,b2,b3)
    def scalene: Boolean = ( b1 != b2 && b1 != b3 && b2 != b3) &&  canBuildTriangle(b1,b2,b3) && allPositive(b1,b2,b3)
    def canBuildTriangle(b1:Double,b2:Double,b3:Double) =  b1 + b2 >= b3 && b1+b3 >= b2 && b2+b3 >= b1
    def allPositive(b :Double*) = b.forall(_ > 0)
}

object Triangle {
    def apply(b1:Int, b2:Int, b3:Int) :Triangle = Triangle(b1.toDouble,b2.toDouble,b3.toDouble)
}