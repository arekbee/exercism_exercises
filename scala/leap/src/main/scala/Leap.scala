object Leap {
  def isDiv(year :Int, by:Int ) :Boolean = year % by == 0
  def leapYear(year: Int): Boolean = 
       (isDiv(year, 4), isDiv(year, 100), isDiv(year, 400)) match {
         case (false, _, _) => false
         case (true, false, _) => true
         case (true, true, false) => false
         case _ => true
       }
}
