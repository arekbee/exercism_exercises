object SecretHandshake {
    val handshake = List("wink", "double blink","close your eyes", "jump").zipWithIndex.map(x=>Math.pow(2, x._2).toInt -> x._1)

    def commands(n :Int) :Seq[String] = handshake.filter{case (x,_)=> (x & n) == x}.sortWith{
               case ((x1,_),(x2,_)) => if (n>=16) x1 > x2 else x1 < x2 
               }.map(_._2) 
}