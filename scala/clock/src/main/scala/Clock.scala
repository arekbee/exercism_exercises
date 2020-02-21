sealed case class RClock(hh:Int, mm:Int) 
object Clock {
    def apply(mm:Int) :Clock  = Clock(0, mm)
}

case class Clock(hh :Int, mm:Int) {
    private val adddedHH = math.floor(mm.toDouble / 60).toInt 
    val c = RClock( ((hh+adddedHH) % 12 + 12) % 12, (mm%60 + 60) % 60 )
    
    override def hashCode() :Int = c.hashCode()
    override def equals(that :Any) : Boolean = 
    that match {
        case that :Clock => this.c == that.c
        case _ => false
    } 
    def +(that :Clock)  = Clock(this.hh + that.hh,this.mm + that.mm )
    def -(that :Clock)  = Clock(this.hh - that.hh,this.mm - that.mm )

}