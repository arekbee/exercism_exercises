object ArmstrongNumbers{

    def isArmstrongNumber(n:Int) : Boolean = {
            val dig = n.toString().size
            n.toString().split("").map(x=> math.pow(x.toInt, dig)).reduce(_+_).toInt == n
    }
}
