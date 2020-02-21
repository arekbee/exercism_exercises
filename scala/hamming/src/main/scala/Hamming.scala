
object Hamming {
    def distance(str1:String, str2:String) :Option[Int] ={
         (str1, str2) match {
        case (s1,s2) if s1.size == s2.size => Some(s1.zip(s2).count(x=>x._1 != x._2))
        case _ => None
        }
    }
}