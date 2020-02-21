object RunLengthEncoding {
    def encode( str:String) :String = {
        str.split("").foldLeft (List.empty[(String, Int)] ) ((acc,x) => 
            acc match {
                case (a,c) ::as if a == x =>  (a,c+1)::as
                case _ => (x,1)::acc
            }
        ).reverse.map(x=> if(x._2==1) x._1 else x._2 + x._1).mkString 
    }

    private def isNumber(str:String) :Boolean = str matches "\\d+"

    def decode( str:String) :String = {
        str.split("").map( (_,1)).foldLeft(List.empty[(String, Int)] ) ( (acc,x) => 
            (acc, isNumber(x._1) ) match {
                case (a::as, false) if (isNumber(a._1)) => (x._1, a._1.toInt) ::as 
                case (a::as, true) if  (isNumber(a._1)) =>  
                    val nr = (a._1.toInt*10+x._1.toInt) 
                    (nr.toString,nr)::as
                case (_, _) =>  x ::acc
            }
        ).reverse.map(x=>x._1 * x._2).mkString
    }

}


