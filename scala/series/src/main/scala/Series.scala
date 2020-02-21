object Series {
    val r = raw"\d+" 

    def slices(n:Int, str:String) :List[List[Int]] = str.split("").filter(_.matches(r)).map(_.toInt).toList.sliding(n).toList
}