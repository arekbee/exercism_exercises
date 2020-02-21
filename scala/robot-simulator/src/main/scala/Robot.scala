
object Bearing extends Enumeration {
    sealed trait Bearing
    case object North extends Bearing
    case object South extends Bearing
    case object East extends Bearing
    case object West extends Bearing
}


case class Robot( bearing: Bearing.Bearing, coordinates : (Int,Int)) {
    def turnRight : Robot ={
        (bearing, coordinates) match {
            case (Bearing.North, cor) => Robot(Bearing.East, cor)
            case (Bearing.South, cor) => Robot(Bearing.West, cor)
            case (Bearing.East, cor) => Robot(Bearing.South, cor)
            case (Bearing.West, cor) => Robot(Bearing.North, cor)
        }
    }
    def turnLeft : Robot ={
        (bearing, coordinates) match {
            case (Bearing.North, cor) => Robot(Bearing.West, cor)
            case (Bearing.South, cor) => Robot(Bearing.East, cor)
            case (Bearing.East, cor) => Robot(Bearing.North, cor)
            case (Bearing.West, cor) => Robot(Bearing.South, cor)
        }
    }
    def advance : Robot = {
        val (x,y)  = coordinates
        bearing match {
            case Bearing.North => Robot(Bearing.North, (x,y+1))
            case Bearing.South => Robot(Bearing.South, (x,y-1))
            case Bearing.East => Robot(Bearing.East, (x+1,y))
            case Bearing.West => Robot(Bearing.West, (x-1,y))
        }
    }

    def simulate(commands:String) : Robot  = {
        def simulateRec(state: Robot, cs:List[String] ) : Robot = {
            cs match {
                case "R"::res =>  simulateRec(state.turnRight, res)
                case "L"::res =>  simulateRec(state.turnLeft, res)
                case "A"::res =>  simulateRec(state.advance, res)
                case Nil | _ => state
            }
        }
        simulateRec(this, commands.split("").toList)
    } 
}