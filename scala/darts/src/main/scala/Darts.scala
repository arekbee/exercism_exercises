object Darts {
    def score(x:Double, y:Double) :Int = {
        math.sqrt(x*x + y*y) match {
            case r if r > 10.0 => 0
            case r if r > 5 => 1
            case r if r > 1 => 5
            case r  => 10
        }
    }
    //def score(x:Int, y:Int) :Int = score(x.toDouble, y.toDouble)
}