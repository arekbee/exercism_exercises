trait Garden
import Plant._

object Garden {
    def defaultGarden(str:String) :  Garden = {

    }

    def plants(child:String)  : List[Plant] = {

    }
}


object Plant extends Enumeration {
    val Violets, Clover, Radishes, Clover = Value
} 