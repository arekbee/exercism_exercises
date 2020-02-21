case class Queen(x:Int, y:Int)
object Queen {
    def create(x:Int, y:Int) = (x,y) match {
        case (x,y) if x>=0 && y>=0 && x<8 && y<8=> Some(Queen(x,y))
        case _ => None
    } 
    
    }



object QueenAttack {
    def canAttack(q1: Queen, q2:Queen) = q1 match {
        case Queen(x,y) if x == q2.x => true
        case Queen(x,y) if y == q2.y => true
        case Queen(x,y) if x - q2.x == y-q2.y => true
        case Queen(x,y) if x - q2.x == q2.y -y => true
        case _ => false
    }
}